{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TestHandler where

import           ClassyPrelude.Yesod

import           Yesod.Filter.TH

import           TestFoundation
import           TestModel


$(mkFilterGenerator "Foo" defaultOptions
    { filtering = defaultFiltering
        { filterDefs =
            [ FilterDef "ctxt" [AutoParam EqOp]
            , FilterDef "cint" defaultFilterParams
            , FilterDef "mint" (AutoParam IsNullOp : defaultFilterParams)
            ]
        }
    , sorting = defaultSorting
        { sortFields = ["cint", "cdbl", "mint", "mdbl"]
        }
    }
 )

getFoosR :: Handler Value
getFoosR = do
    filters' <- $(mkFilters)
    selectOpts <- $(mkSelectOpts)
    foos <- runDB $ selectList filters' selectOpts
    returnJson foos
