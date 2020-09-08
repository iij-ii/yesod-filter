{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Yesod.Filter.THSpec (spec) where

import           Data.Text              (Text)
import           Test.Hspec
import           Yesod.Persist          (Filter, SelectOpt (Asc, Desc), (!=.), (<.), (<=.), (==.),
                                         (>.), (>=.))

import           Yesod.Filter.TH

import           Yesod.Filter.TestTypes


$(mkToFilterValueInstances "Foo")

spec :: Spec
spec = do
    describe "availableFiltersE" $ do
        it "returns [] for defaultFiltering" $ do
            let availableFilters = $(availableFiltersE "Foo" defaultFiltering)
            buildFilters "foo" availableFilters `shouldSatisfy` null

        it "returns [(\"ctxt\", Just $ FooCtxt ==. \"foo\")] for defaultFiltering {filterDefs = [FilterDef \"ctxt\" [AutoParam EqOp]]}" $ do
            let availableFilters = $(availableFiltersE "Foo" $ defaultFiltering {filterDefs = [FilterDef "ctxt" [AutoParam EqOp]]})
            buildFilters "foo" availableFilters `shouldBe` [("ctxt", Just $ FooCtxt ==. "foo")]

        it "returns [(\"ctxt\", Just $ FooCtxt ==. \"foo\")] for SimpleFiltering [FilterDef \"ctxt\" [AutoParam EqOp]" $ do
            let availableFilters = $(availableFiltersE "Foo" $ SimpleFiltering [FilterDef "ctxt" [AutoParam EqOp]])
            buildFilters "foo" availableFilters `shouldBe` [("ctxt", Just $ FooCtxt ==. "foo")]

        it "returns [] for NoFiltering" $ do
            let availableFilters = $(availableFiltersE "Foo" NoFiltering)
            buildFilters "foo" availableFilters `shouldSatisfy` null

        it "returns filters consists of all operators other than IsNullOp for defaultFilterParams" $ do
            let availableFilters = $(availableFiltersE "Foo" $ defaultFiltering { filterDefs =
                    [ FilterDef "ctxt" defaultFilterParams
                    , FilterDef "mtxt" defaultFilterParams
                    ]})
            buildFilters "foo" availableFilters `shouldMatchList`
                [ ("ctxt",     Just $ FooCtxt ==. "foo")
                , ("ctxt__ne", Just $ FooCtxt !=. "foo")
                , ("ctxt__gt", Just $ FooCtxt  >. "foo")
                , ("ctxt__lt", Just $ FooCtxt  <. "foo")
                , ("ctxt__ge", Just $ FooCtxt >=. "foo")
                , ("ctxt__le", Just $ FooCtxt <=. "foo")
                , ("mtxt",     Just $ FooMtxt ==. Just "foo")
                , ("mtxt__ne", Just $ FooMtxt !=. Just "foo")
                , ("mtxt__gt", Just $ FooMtxt  >. Just "foo")
                , ("mtxt__lt", Just $ FooMtxt  <. Just "foo")
                , ("mtxt__ge", Just $ FooMtxt >=. Just "foo")
                , ("mtxt__le", Just $ FooMtxt <=. Just "foo")
                ]

        it "returns filters with the parameter name appended with the default suffix for AutoParam" $ do
            let availableFilters = $(availableFiltersE "Foo" $ defaultFiltering { filterDefs =
                    [ FilterDef "mbol"
                        [ AutoParam EqOp
                        , AutoParam NeOp
                        , AutoParam GtOp
                        , AutoParam LtOp
                        , AutoParam GeOp
                        , AutoParam LeOp
                        , AutoParam IsNullOp
                        ]]})
            buildFilters "True" availableFilters `shouldBe`
                [ ("mbol",         Just $ FooMbol ==. Just True)
                , ("mbol__ne",     Just $ FooMbol !=. Just True)
                , ("mbol__gt",     Just $ FooMbol  >. Just True)
                , ("mbol__lt",     Just $ FooMbol  <. Just True)
                , ("mbol__ge",     Just $ FooMbol >=. Just True)
                , ("mbol__le",     Just $ FooMbol <=. Just True)
                , ("mbol__isnull", Just $ FooMbol ==. Nothing)
                ]

        it "returns filters with the specified parameter name for CustomParam" $ do
            let availableFilters = $(availableFiltersE "Foo" $ defaultFiltering { filterDefs =
                    [ FilterDef "mbol"
                        [ CustomParam EqOp     "foo"
                        , CustomParam NeOp     "bar"
                        , CustomParam GtOp     "baz"
                        , CustomParam LtOp     "qux"
                        , CustomParam GeOp     "quux"
                        , CustomParam LeOp     "corge"
                        , CustomParam IsNullOp "grault"
                        ]]})
            buildFilters "True" availableFilters `shouldBe`
                [ ("foo",    Just $ FooMbol ==. Just True)
                , ("bar",    Just $ FooMbol !=. Just True)
                , ("baz",    Just $ FooMbol  >. Just True)
                , ("qux",    Just $ FooMbol  <. Just True)
                , ("quux",   Just $ FooMbol >=. Just True)
                , ("corge",  Just $ FooMbol <=. Just True)
                , ("grault", Just $ FooMbol ==. Nothing)
                ]

    describe "defaultOrderByE" $ do
        it "returns Nothing for defaultSorting" $ do
            let defaultOrderBy = $(defaultOrderByE "Foo" defaultSorting)
            defaultOrderBy `shouldBe` (Nothing :: Maybe (SelectOpt Foo))

        it "returns Just (Asc FooCtxt) for defaultSorting {ORDERBY \"ctxt\" ASC}" $ do
            let defaultOrderBy = $(defaultOrderByE "Foo" defaultSorting { defaultOrdering = ORDERBY "ctxt" ASC })
            defaultOrderBy `shouldBe` Just (Asc FooCtxt)

        it "returns Just (Desc FooCtxt) for AllowSorting \"\" [] $ ORDERBY \"ctxt\" DESC" $ do
            let defaultOrderBy = $(defaultOrderByE "Foo" $ AllowSorting "" [] $ ORDERBY "ctxt" DESC)
            defaultOrderBy `shouldBe` Just (Desc FooCtxt)

        it "returns Nothing for AllowSorting \"\" [] NaturalOrdering" $ do
            let defaultOrderBy = $(defaultOrderByE "Foo" $ AllowSorting "" [] NaturalOrdering)
            defaultOrderBy `shouldBe` (Nothing :: Maybe (SelectOpt Foo))

        it "returns Nothing for DisallowSorting" $ do
            let defaultOrderBy = $(defaultOrderByE "Foo" DisallowSorting)
            defaultOrderBy `shouldBe` (Nothing :: Maybe (SelectOpt Foo))

    describe "availableOrderBysE" $ do
        it "returns [] for defaultSorting" $ do
            let availableOrderBys = $(availableOrderBysE "Foo" defaultSorting)
            availableOrderBys `shouldBe` ([] :: [(Text, SelectOpt Foo)])

        it "returns [(\"ctxt\", Asc FooCtxt), (\"-ctxt\", Desc FooCtxt)] for defaultSorting { sortFields = [\"ctxt\"] }" $ do
            let availableOrderBys = $(availableOrderBysE "Foo" defaultSorting { sortFields = ["ctxt"] })
            availableOrderBys `shouldBe` [("ctxt", Asc FooCtxt), ("-ctxt", Desc FooCtxt)]

        it "returns [(\"ctxt\", Asc FooCtxt), (\"-ctxt\", Desc FooCtxt)] for AllowSorting \"\" [\"ctxt\"] NaturalOrdering" $ do
            let availableOrderBys = $(availableOrderBysE "Foo" $ AllowSorting "" ["ctxt"] NaturalOrdering)
            availableOrderBys `shouldBe` [("ctxt", Asc FooCtxt), ("-ctxt", Desc FooCtxt)]

        it "returns [] for sortFields = []" $ do
            let availableOrderBys = $(availableOrderBysE "Foo" defaultSorting { sortFields = [] })
            availableOrderBys `shouldBe` ([] :: [(Text, SelectOpt Foo)])

        it "returns multiple orderBys for multiple sort fields" $ do
            let availableOrderBys = $(availableOrderBysE "Foo" defaultSorting { sortFields = ["ctxt", "cint", "mday"] })
            availableOrderBys `shouldMatchList`
                [ ("ctxt", Asc FooCtxt), ("-ctxt", Desc FooCtxt)
                , ("cint", Asc FooCint), ("-cint", Desc FooCint)
                , ("mday", Asc FooMday), ("-mday", Desc FooMday)
                ]

        it "returns [] for DisallowSorting" $ do
            let availableOrderBys = $(availableOrderBysE "Foo" DisallowSorting)
            availableOrderBys `shouldBe` ([] :: [(Text, SelectOpt Foo)])



buildFilters :: Text -> [(Text, Text -> Maybe (Filter Foo))] -> [(Text, Maybe (Filter Foo))]
buildFilters v = map (\(p, f) -> (p, f v))
