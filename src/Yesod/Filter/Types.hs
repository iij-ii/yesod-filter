{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Filter.Types where

import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)


-- | A comparison operator to apply to the filter.
data FilterOp
    = EqOp      -- ^ =
    | NeOp      -- ^ \<\>
    | GtOp      -- ^ \>
    | LtOp      -- ^ \<
    | GeOp      -- ^ \>=
    | LeOp      -- ^ \<=
    | IsNullOp  -- ^ IS NULL
    deriving (Lift, Show)

-- | A parameter name used to specify the filter.
data FilterParam
    -- | Use automatically generated parameter names.
    = AutoParam FilterOp
    -- | Use custom parameter name.
    | CustomParam FilterOp Text deriving (Lift, Show)

-- | A filter defintion.
data FilterDef
    = FilterDef
        Text            -- ^ A field name used for the filter.
        [FilterParam]   -- ^ Parameters used to specify the filter.
        deriving (Lift, Show)

-- | A filtering setting.
data Filtering
    -- | Allow users to specify filtering by query parameters.
    = SimpleFiltering
        { filterDefs :: [FilterDef]
        }
    -- TODO: add ComplexFiltering
    -- | Disable filtering.
    | NoFiltering
    deriving (Lift, Show)

data SortDirection = ASC | DESC deriving (Lift, Show)

-- | A value that becomes the SQL ORDER BY clause.
data SortOrdering = NaturalOrdering | ORDERBY Text SortDirection deriving (Lift, Show)

-- | A sorting setting.
data Sorting
    -- | Allow users to specify sort order by query parameters.
    = AllowSorting
        { sortParam       :: Text           -- ^ A parameter name to specify the field name as a sort key.
        , sortFields      :: [Text]         -- ^ Field names that can be used as sort keys.
        , defaultOrdering :: SortOrdering   -- ^ A default order setting.
        }
    -- | Disable sorting.
    | DisallowSorting
     deriving (Lift, Show)

-- | A value that becomes the SQL OFFSET clause.
data PageOffset = NoOffset | OFFSET Int deriving (Lift, Show)

-- | A value that becomes the SQL LIMIT clause.
data PageLimit = NoLimit | LIMIT Int deriving (Lift, Show)

-- | A pagination setting. Currently, only offset pagination is available.
data Pagination
    -- | Allow users to specify offset pagination by query parameters.
    = OffsetPagination
        { offsetParam   :: Text         -- ^ A parameter name to specify the offset value.
        , defaultOffset :: PageOffset   -- ^ A default offset setting.
        , limitParam    :: Text         -- ^ A parameter name to specify the limit value.
        , defaultLimit  :: PageLimit    -- ^ A default limit setting.
        }
    -- TODO: add CursorPagination
    -- | Disable pagination.
    | NoPagination
    deriving (Lift, Show)

-- | Options to specify filtering, sorting, and pagination settings to generate.
data Options = Options
    { filtering  :: Filtering
    , sorting    :: Sorting
    , pagination :: Pagination
    } deriving (Lift, Show)

-- | Default filter parameters.
defaultFilterParams :: [FilterParam]
defaultFilterParams =
    [ AutoParam EqOp
    , AutoParam NeOp
    , AutoParam GtOp
    , AutoParam LtOp
    , AutoParam GeOp
    , AutoParam LeOp
    ]

-- | A default filtering setting.
defaultFiltering :: Filtering
defaultFiltering = SimpleFiltering []

-- | A default sorting setting.
defaultSorting :: Sorting
defaultSorting = AllowSorting
    { sortParam = "sort"
    , sortFields = []
    , defaultOrdering = NaturalOrdering
    }

-- | A default pagination setting.
defaultPagination :: Pagination
defaultPagination = OffsetPagination
    { offsetParam = "offset"
    , defaultOffset = NoOffset
    , limitParam = "limit"
    , defaultLimit = NoLimit
    }

-- | Default options.
defaultOptions :: Options
defaultOptions = Options
    { filtering = defaultFiltering
    , sorting = defaultSorting
    , pagination = defaultPagination
    }
