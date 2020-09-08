{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Filter.Types where

import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)


data FilterOp
    = EqOp
    | NeOp
    | GtOp
    | LtOp
    | GeOp
    | LeOp
    | IsNullOp
    deriving (Lift, Show)

data FilterParam = AutoParam FilterOp | CustomParam FilterOp Text deriving (Lift, Show)

data FilterDef = FilterDef Text [FilterParam] deriving (Lift, Show)

data Filtering
    = SimpleFiltering
        { filterDefs :: [FilterDef]
        }
    -- | ComplexFiltering ...
    | NoFiltering
    deriving (Lift, Show)

data SortDirection = ASC | DESC deriving (Lift, Show)

data SortOrdering = NaturalOrdering | ORDERBY Text SortDirection deriving (Lift, Show)

data Sorting
    = AllowSorting
        { sortParam       :: Text
        , sortFields      :: [Text]
        , defaultOrdering :: SortOrdering
        }
    | DisallowSorting
     deriving (Lift, Show)

data PageOffset = NoOffset | OFFSET Int deriving (Lift, Show)

data PageLimit = NoLimit | LIMIT Int deriving (Lift, Show)

data Pagination
    = OffsetPagination
        { offsetParam   :: Text
        , defaultOffset :: PageOffset
        , limitParam    :: Text
        , defaultLimit  :: PageLimit
        }
    -- | CursorPagination ...
    | NoPagination
    deriving (Lift, Show)

data Options = Options
    { filtering  :: Filtering
    , sorting    :: Sorting
    , pagination :: Pagination
    } deriving (Lift, Show)

defaultFilterParams :: [FilterParam]
defaultFilterParams =
    [ AutoParam EqOp
    , AutoParam NeOp
    , AutoParam GtOp
    , AutoParam LtOp
    , AutoParam GeOp
    , AutoParam LeOp
    ]

defaultFiltering :: Filtering
defaultFiltering = SimpleFiltering []

defaultSorting :: Sorting
defaultSorting = AllowSorting
    { sortParam = "sort"
    , sortFields = []
    , defaultOrdering = NaturalOrdering
    }

defaultPagination :: Pagination
defaultPagination = OffsetPagination
    { offsetParam = "offset"
    , defaultOffset = NoOffset
    , limitParam = "limit"
    , defaultLimit = NoLimit
    }

defaultOptions :: Options
defaultOptions = Options
    { filtering = defaultFiltering
    , sorting = defaultSorting
    , pagination = defaultPagination
    }
