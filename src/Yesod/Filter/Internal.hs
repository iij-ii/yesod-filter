{-# LANGUAGE OverloadedStrings #-}

module Yesod.Filter.Internal where

import           Data.Text          (Text, unpack)
import           Yesod.Persist      (Filter, SelectOpt (LimitTo, OffsetBy))

import           Yesod.Filter.Read  (readMaybeInt)
import           Yesod.Filter.Types


getFilters :: [(Text, Text -> Maybe (Filter record))] -> [(Text, Text)] -> [Either Text (Filter record)]
getFilters availableFilters = map (uncurry getFilter)
  where
    getFilter paramName paramValue = case lookup paramName availableFilters of
        Just filterBuilder -> case filterBuilder paramValue of
            Just filt -> Right filt
            Nothing   -> Left $ "Invalid filter value: " <> paramValue
        Nothing            -> Left $ "Invalid filter name: " <> paramName

getOrderBy
    :: Maybe (SelectOpt record)
    -> [(Text, SelectOpt record)]
    -> Maybe Text
    -> Either Text (Maybe (SelectOpt record))
getOrderBy defaultOrderBy availableOrderBys msort = case msort of
    Just sort' -> case lookup sort' availableOrderBys of
        Just orderBy -> Right $ Just orderBy
        Nothing      -> Left $ "Invalid sort option: " <> sort'
    Nothing    -> Right defaultOrderBy

getOffsetBy :: PageOffset -> Maybe Text -> Either Text (Maybe (SelectOpt record))
getOffsetBy defaultOffset' moffset = case moffset of
    Just offset -> case readMaybeInt (unpack offset) of
        Just n  -> Right $ Just $ OffsetBy n
        Nothing -> Left $ "Invalid offset option: " <> offset
    Nothing     -> case defaultOffset' of
        OFFSET n -> Right $ Just $ OffsetBy n
        NoOffset -> Right Nothing

getLimitTo :: PageLimit -> Maybe Text -> Either Text (Maybe (SelectOpt record))
getLimitTo defaultLimit' mlimit = case mlimit of
    Just limit -> case readMaybeInt (unpack limit) of
        Just n  -> Right $ Just $ LimitTo n
        Nothing -> Left $ "Invalid limit option: " <> limit
    Nothing    -> case defaultLimit' of
        LIMIT n -> Right $ Just $ LimitTo n
        NoLimit -> Right Nothing
