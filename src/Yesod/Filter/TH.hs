{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Yesod.Filter.TH
    ( mkFilterGenerator
    , mkFilters
    , mkSelectOpts
    -- Types
    , FilterOp (..)
    , FilterParam (..)
    , FilterDef (..)
    , Filtering (..)
    , SortDirection (..)
    , SortOrdering (..)
    , Sorting (..)
    , PageOffset (..)
    , PageLimit (..)
    , Pagination (..)
    , Options (..)
    , defaultFilterParams
    , defaultFiltering
    , defaultSorting
    , defaultPagination
    , defaultOptions
    -- for testing
    , mkToFilterValueInstances
    , availableFiltersE
    , defaultOrderByE
    , availableOrderBysE
    ) where

import           Control.Monad        ((>=>))
import           Data.Text            (Text, cons, pack, unpack)
import           Data.Time            (Day, TimeOfDay, UTCTime)
import           Database.Persist     (BackendKey)
import           Database.Persist.Sql (SqlBackend)
import           Language.Haskell.TH  (DecsQ, ExpQ, conE, conT, listE, mkName)
import           Text.Read            (readMaybe)
import           Yesod.Core           (MonadHandler)
import           Yesod.Persist        (Filter, Key, SelectOpt (Asc, Desc), (!=.), (<.), (<=.),
                                       (==.), (>.), (>=.))

import           Yesod.Filter.Builder (buildFiltersFromGetParams, buildSelectOptsFromGetParams)
import           Yesod.Filter.Read    (capitalize, readMaybeBool, readMaybeDay, readMaybeDouble,
                                       readMaybeInt, readMaybeTimeOfDay, readMaybeUTCTime)
import           Yesod.Filter.Types


-- ExpQ: [| [Filter record] |]
mkFilters :: ExpQ
mkFilters = [| filtersFromGetParams |]

-- ExpQ: [| [SelectOpt record] |]
mkSelectOpts :: ExpQ
mkSelectOpts = [| selectOptsFromGetParams |]

mkFilterGenerator :: Text -> Options -> DecsQ
mkFilterGenerator model options = concat <$> sequence
    [ mkToFilterValueInstances model
    , mkFiltersFromGetParams model options
    , mkSelectOptsFromGetParams model options
    ]

mkToFilterValueInstances :: Text -> DecsQ
mkToFilterValueInstances model = [d|
    class ToKey a where
        toKey :: BackendKey SqlBackend -> Key a

    instance ToKey $(conT $ mkName (unpack model)) where
        toKey = $(conE $ mkName $ unpack model ++ "Key")

    class ToFilterValue a where
        toFilterValue :: Text -> Maybe a

    instance ToKey record => ToFilterValue (Key record) where
        toFilterValue v = case readMaybe (unpack v) of
            Just n  -> Just (toKey n)
            Nothing -> Nothing

    instance ToFilterValue Text where
        toFilterValue = Just

    instance ToFilterValue Int where
        toFilterValue = readMaybeInt . unpack

    instance ToFilterValue Double where
        toFilterValue = readMaybeDouble . unpack

    instance ToFilterValue Bool where
        toFilterValue = readMaybeBool . unpack

    instance ToFilterValue Day where
        toFilterValue = readMaybeDay . unpack

    instance ToFilterValue TimeOfDay where
        toFilterValue = readMaybeTimeOfDay . unpack

    instance ToFilterValue UTCTime where
        toFilterValue = readMaybeUTCTime . unpack

    instance ToFilterValue a => ToFilterValue (Maybe a) where
        toFilterValue = toFilterValue >=> Just . Just
    |]

mkFiltersFromGetParams :: Text -> Options -> DecsQ
mkFiltersFromGetParams model options = [d|
    filtersFromGetParams :: MonadHandler m => m [Filter $(conT $ mkName (unpack model))]
    filtersFromGetParams = buildFiltersFromGetParams
        $(availableFiltersE model $ filtering options)
        $([| options |])
    |]

-- ExpQ: [| [(Text, Text -> Maybe (Filter record))] |]
availableFiltersE :: Text -> Filtering -> ExpQ
availableFiltersE model (SimpleFiltering defs) = [| $(listE $ concatMap availableFilterE' defs) |]
  where
    availableFilterE' (FilterDef field filterParams) = map (availableFilterE model field) filterParams
availableFiltersE _     _                      = [| [] |]

-- ExpQ: [| (Text, Text -> Maybe (Filter record)) |]
availableFilterE :: Text -> Text -> FilterParam -> ExpQ
availableFilterE model field (CustomParam op param) = [| (param,        $(filterBuilderE model field op)) |]
availableFilterE model field (AutoParam op)         = [| (defaultParam, $(filterBuilderE model field op)) |]
  where
    defaultParam = case op of
        EqOp     -> field
        NeOp     -> field <> pack "__ne"
        GtOp     -> field <> pack "__gt"
        LtOp     -> field <> pack "__lt"
        GeOp     -> field <> pack "__ge"
        LeOp     -> field <> pack "__le"
        IsNullOp -> field <> pack "__isnull"

-- ExpQ: [| Text -> Maybe (Filter record) |]
filterBuilderE :: Text -> Text -> FilterOp -> ExpQ
filterBuilderE model field EqOp     = [| toFilterValue >=> (Just . (==.) $(entityFieldE model field)) |]
filterBuilderE model field NeOp     = [| toFilterValue >=> (Just . (!=.) $(entityFieldE model field)) |]
filterBuilderE model field GtOp     = [| toFilterValue >=> (Just .  (>.) $(entityFieldE model field)) |]
filterBuilderE model field LtOp     = [| toFilterValue >=> (Just .  (<.) $(entityFieldE model field)) |]
filterBuilderE model field GeOp     = [| toFilterValue >=> (Just . (>=.) $(entityFieldE model field)) |]
filterBuilderE model field LeOp     = [| toFilterValue >=> (Just . (<=.) $(entityFieldE model field)) |]
filterBuilderE model field IsNullOp = [|
        toFilterValue >=> (\b -> Just $ (if b then (==.) else (!=.)) $(entityFieldE model field) Nothing)
    |]

mkSelectOptsFromGetParams :: Text -> Options -> DecsQ
mkSelectOptsFromGetParams model options = [d|
    selectOptsFromGetParams :: MonadHandler m => m [SelectOpt $(conT $ mkName (unpack model))]
    selectOptsFromGetParams = buildSelectOptsFromGetParams
        $(defaultOrderByE model $ sorting options)
        $(availableOrderBysE model $ sorting options)
        $([| options |])
    |]

-- ExpQ: [| Maybe (SelectOpt record) |]
defaultOrderByE :: Text -> Sorting -> ExpQ
defaultOrderByE model (AllowSorting _ _ (ORDERBY field ASC))  = [| Just $ Asc  $(entityFieldE model field) |]
defaultOrderByE model (AllowSorting _ _ (ORDERBY field DESC)) = [| Just $ Desc $(entityFieldE model field) |]
defaultOrderByE _     _                                       = [| Nothing |]

-- ExpQ: [| [(Text, SelectOpt record)] |]
availableOrderBysE :: Text -> Sorting -> ExpQ
availableOrderBysE model (AllowSorting _ fields _) = [| $(listE $ map asc fields) ++ $(listE $ map desc fields) |]
  where
    asc  field = [| (field,          Asc  $(entityFieldE model field)) |]
    desc field = [| (cons '-' field, Desc $(entityFieldE model field)) |]
availableOrderBysE _ _                             = [| [] |]

-- ExpQ: [| EntityField record typ |]
entityFieldE :: Text -> Text-> ExpQ
entityFieldE model field = conE $ mkName $ unpack model ++ capitalize (unpack field)
