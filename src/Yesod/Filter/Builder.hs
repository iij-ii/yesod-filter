module Yesod.Filter.Builder where

import           Control.Exception     (throwIO)
import           Data.Either           (lefts, rights)
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text)
import           Yesod.Core            (ErrorResponse (InvalidArgs), MonadHandler, getRequest,
                                        liftIO, lookupGetParam, reqGetParams)
import           Yesod.Core.Types      (HandlerContents (HCError))
import           Yesod.Persist         (Filter, SelectOpt)

import           Yesod.Filter.Internal (getFilters, getLimitTo, getOffsetBy, getOrderBy)
import           Yesod.Filter.Types


buildFiltersFromGetParams
    :: MonadHandler m
    => [(Text, Text -> Maybe (Filter record))]
    -> Options
    -> m [Filter record]
buildFiltersFromGetParams availableFilters options = do
    request <- getRequest
    let
        otherParams = sortParams (sorting options) ++ pageParams (pagination options)
        filterParams = filter ((`notElem` otherParams) . fst) $ reqGetParams request
        efilters = getFilters availableFilters filterParams
    case lefts efilters of
        []      -> pure $ rights efilters
        msg : _ -> liftIO $ throwIO $ HCError $ InvalidArgs [msg]
  where
    sortParams (AllowSorting sortParam' _ _) = [sortParam']
    sortParams _                             = []
    pageParams (OffsetPagination offsetParam' _ limitParam' _) = [offsetParam', limitParam']
    pageParams _                                               = []


buildSelectOptsFromGetParams
    :: MonadHandler m
    => Maybe (SelectOpt record)
    -> [(Text, SelectOpt record)]
    -> Options
    -> m [SelectOpt record]
buildSelectOptsFromGetParams defaultOrderBy availableOrderBys options = do
    orderBy <- getOrderByFromGetParams $ sorting options
    offsetBy <- getOffsetByFromGetParams $ pagination options
    limitTo <- getLimitToFromGetParams $ pagination options
    pure $ catMaybes [orderBy, offsetBy, limitTo]
  where
    getOrderByFromGetParams (AllowSorting sortParam' _ _) = do
        msort <- lookupGetParam sortParam'
        case getOrderBy defaultOrderBy availableOrderBys msort of
            Right morderBy -> pure morderBy
            Left msg       -> liftIO $ throwIO $ HCError $ InvalidArgs [msg]
    getOrderByFromGetParams _ = pure Nothing

    getOffsetByFromGetParams (OffsetPagination offsetParam' defaultOffset' _ _) = do
        moffset <- lookupGetParam offsetParam'
        case getOffsetBy defaultOffset' moffset of
            Right moffsetBy -> pure moffsetBy
            Left msg        -> liftIO $ throwIO $ HCError $ InvalidArgs [msg]
    getOffsetByFromGetParams _ = pure Nothing

    getLimitToFromGetParams (OffsetPagination _ _ limitParam' defaultLimit') = do
        mlimit <- lookupGetParam limitParam'
        case getLimitTo defaultLimit' mlimit of
            Right mlimitTo -> pure mlimitTo
            Left msg       -> liftIO $ throwIO $ HCError $ InvalidArgs [msg]
    getLimitToFromGetParams _ = pure Nothing
