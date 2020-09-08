{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Yesod.Filter.InternalSpec (spec) where

import           Control.Monad          (forM_, (>=>))
import           Data.Either            (isLeft, partitionEithers)
import           Data.Text              (Text)
import           Data.Time              (TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian,
                                         timeOfDayToTime)
import           Database.Persist       (Filter, SelectOpt (Asc, Desc, LimitTo, OffsetBy), (!=.),
                                         (<.), (<=.), (==.), (>.), (>=.))
import           Test.Hspec

import           Yesod.Filter.Internal
import           Yesod.Filter.TH        (PageLimit (LIMIT, NoLimit), PageOffset (NoOffset, OFFSET),
                                         mkToFilterValueInstances)

import           Yesod.Filter.TestTypes


$(mkToFilterValueInstances "Foo")

spec :: Spec
spec = do
    describe "getFilters" $ do
        context "for not nullable fields" $ do
            let tests =
                    [   ( [("ctxt", toFilterValue >=> (Just . (==.) FooCtxt))]
                        , [("ctxt", "foo")]
                        , [Right (FooCtxt ==. "foo")]
                        )
                    ,   ( [("cint", toFilterValue >=> (Just . (==.) FooCint))]
                        , [("cint", "1")]
                        , [Right (FooCint ==. 1)]
                        )
                    ,   ( [("cdbl", toFilterValue >=> (Just . (==.) FooCdbl))]
                        , [("cdbl", "1.0")]
                        , [Right (FooCdbl ==. 1.0)]
                        )
                    ,   ( [("cbol", toFilterValue >=> (Just . (==.) FooCbol))]
                        , [("cbol", "true")]
                        , [Right (FooCbol ==. True)]
                        )
                    ,   ( [("cday", toFilterValue >=> (Just . (==.) FooCday))]
                        , [("cday", "2020-01-01")]
                        , [Right (FooCday ==. fromGregorian 2020 1 1)]
                        )
                    ,   ( [("ctod", toFilterValue >=> (Just . (==.) FooCtod))]
                        , [("ctod", "12:34:56")]
                        , [Right (FooCtod ==. TimeOfDay 12 34 56)]
                        )
                    ,   ( [("cutm", toFilterValue >=> (Just . (==.) FooCutm))]
                        , [("cutm", "2020-01-01T12:34:56Z")]
                        , [Right (FooCutm ==. UTCTime (fromGregorian 2020 1 1 ) (timeOfDayToTime $ TimeOfDay 12 34 56))]
                        )
                    ]
            forM_ tests $ \(availableFilters, params, ret) ->
                it ("returns " ++ show ret ++ " for " ++ show params) $
                    getFilters availableFilters params `shouldBe` ret

        context "for nullable fields" $ do
            let tests =
                    [   ( [("mtxt", toFilterValue >=> (Just . (==.) FooMtxt))]
                        , [("mtxt", "foo")]
                        , [Right (FooMtxt ==. Just "foo")]
                        )
                    ,   ( [("mint", toFilterValue >=> (Just . (==.) FooMint))]
                        , [("mint", "1")]
                        , [Right (FooMint ==. Just 1)]
                        )
                    ,   ( [("mdbl", toFilterValue >=> (Just . (==.) FooMdbl))]
                        , [("mdbl", "1.0")]
                        , [Right (FooMdbl ==. Just 1.0)]
                        )
                    ,   ( [("mbol", toFilterValue >=> (Just . (==.) FooMbol))]
                        , [("mbol", "true")]
                        , [Right (FooMbol ==. Just True)]
                        )
                    ,   ( [("mday", toFilterValue >=> (Just . (==.) FooMday))]
                        , [("mday", "2020-01-01")]
                        , [Right (FooMday ==. (Just $ fromGregorian 2020 1 1))]
                        )
                    ,   ( [("mtod", toFilterValue >=> (Just . (==.) FooMtod))]
                        , [("mtod", "12:34:56")]
                        , [Right (FooMtod ==. (Just $ TimeOfDay 12 34 56))]
                        )
                    ,   ( [("mutm", toFilterValue >=> (Just . (==.) FooMutm))]
                        , [("mutm", "2020-01-01T12:34:56Z")]
                        , [Right (FooMutm ==. (Just $ UTCTime (fromGregorian 2020 1 1 ) (timeOfDayToTime $ TimeOfDay 12 34 56)))]
                        )
                    ]
            forM_ tests $ \(availableFilters, params, ret) ->
                it ("returns " ++ show ret ++ " for " ++ show params) $
                    getFilters availableFilters params `shouldBe` ret

        context "for not nullable fields with various operators" $ do
            let tests =
                    [   ( [("cint", toFilterValue >=> (Just . (==.) FooCint))]
                        , [("cint", "1")]
                        , [Right (FooCint ==. 1)]
                        )
                    ,   ( [("cint__ne", toFilterValue >=> (Just . (!=.) FooCint))]
                        , [("cint__ne", "1")]
                        , [Right (FooCint !=. 1)]
                        )
                    ,   ( [("cint__gt", toFilterValue >=> (Just .  (>.) FooCint))]
                        , [("cint__gt", "1")]
                        , [Right (FooCint >. 1)]
                        )
                    ,   ( [("cint__lt", toFilterValue >=> (Just .  (<.) FooCint))]
                        , [("cint__lt", "1")]
                        , [Right (FooCint <. 1)]
                        )
                    ,   ( [("cint__ge", toFilterValue >=> (Just . (>=.) FooCint))]
                        , [("cint__ge", "1")]
                        , [Right (FooCint >=. 1)]
                        )
                    ,   ( [("cint__le", toFilterValue >=> (Just . (<=.) FooCint))]
                        , [("cint__le", "1")]
                        , [Right (FooCint <=. 1)]
                        )
                    ]
            forM_ tests $ \(availableFilters, params, ret) ->
                it ("returns " ++ show ret ++ " for " ++ show params) $
                    getFilters availableFilters params `shouldBe` ret

        context "for not nullable fields with various operators" $ do
            let tests =
                    [   ( [("mint", toFilterValue >=> (Just . (==.) FooMint))]
                        , [("mint", "1")]
                        , [Right (FooMint ==. Just 1)]
                        )
                    ,   ( [("mint__ne", toFilterValue >=> (Just . (!=.) FooMint))]
                        , [("mint__ne", "1")]
                        , [Right (FooMint !=. Just 1)]
                        )
                    ,   ( [("mint__gt", toFilterValue >=> (Just .  (>.) FooMint))]
                        , [("mint__gt", "1")]
                        , [Right (FooMint >. Just 1)]
                        )
                    ,   ( [("mint__lt", toFilterValue >=> (Just .  (<.) FooMint))]
                        , [("mint__lt", "1")]
                        , [Right (FooMint <. Just 1)]
                        )
                    ,   ( [("mint__ge", toFilterValue >=> (Just . (>=.) FooMint))]
                        , [("mint__ge", "1")]
                        , [Right (FooMint >=. Just 1)]
                        )
                    ,   ( [("mint__le", toFilterValue >=> (Just . (<=.) FooMint))]
                        , [("mint__le", "1")]
                        , [Right (FooMint <=. Just 1)]
                        )
                    ,   ( [("mint__isnull", toFilterValue >=> (\b -> Just $ (if b then (==.) else (!=.)) FooMint Nothing))]
                        , [("mint__isnull", "true")]
                        , [Right (FooMint ==. Nothing)]
                        )
                    ]
            forM_ tests $ \(availableFilters, params, ret) ->
                it ("returns " ++ show ret ++ " for " ++ show params) $
                    getFilters availableFilters params `shouldBe` ret

        context "for invalid value type" $ do
            let tests =
                    [   ( [("cint", toFilterValue >=> (Just . (==.) FooCint))]
                        , [("cint", "foo")]
                        )
                    ,   ( [("cdbl", toFilterValue >=> (Just . (==.) FooCdbl))]
                        , [("cdbl", "foo")]
                        )
                    ,   ( [("cbol", toFilterValue >=> (Just . (==.) FooCbol))]
                        , [("cbol", "foo")]
                        )
                    ,   ( [("cday", toFilterValue >=> (Just . (==.) FooCday))]
                        , [("cday", "foo")]
                        )
                    ,   ( [("ctod", toFilterValue >=> (Just . (==.) FooCtod))]
                        , [("ctod", "foo")]
                        )
                    ,   ( [("cutm", toFilterValue >=> (Just . (==.) FooCutm))]
                        , [("cutm", "foo")]
                        )
                    ,   ( [("mint", toFilterValue >=> (Just . (==.) FooMint))]
                        , [("mint", "foo")]
                        )
                    ,   ( [("mdbl", toFilterValue >=> (Just . (==.) FooMdbl))]
                        , [("mdbl", "foo")]
                        )
                    ,   ( [("mbol", toFilterValue >=> (Just . (==.) FooMbol))]
                        , [("mbol", "foo")]
                        )
                    ,   ( [("mday", toFilterValue >=> (Just . (==.) FooMday))]
                        , [("mday", "foo")]
                        )
                    ,   ( [("mtod", toFilterValue >=> (Just . (==.) FooMtod))]
                        , [("mtod", "foo")]
                        )
                    ,   ( [("mutm", toFilterValue >=> (Just . (==.) FooMutm))]
                        , [("mutm", "foo")]
                        )
                    ]
            forM_ tests $ \(availableFilters, params) ->
                it ("returns Left for " ++ show params) $
                    getFilters availableFilters params `shouldSatisfy` isALeft

        it "returns [] if params is []" $
            getFilters [("ctxt", toFilterValue >=> (Just . (==.) FooCtxt))] []
                `shouldBe` []

        it "returns Left if the field has no available filter" $
            getFilters [("ctxt", toFilterValue >=> (Just . (==.) FooCtxt))] [("cint", "1")]
                `shouldSatisfy` isALeft

        it "returns multiple filters for multiple parameters" $ do
            let
                availableFilters =
                    [ ("ctxt", toFilterValue >=> (Just . (==.) FooCtxt))
                    , ("cdbl", toFilterValue >=> (Just . (==.) FooCdbl))
                    , ("mint__isnull", toFilterValue >=> (\b -> Just $ (if b then (==.) else (!=.)) FooMint Nothing))
                    ]
                params =
                    [ ("ctxt", "foo")
                    , ("cdbl", "1.0")
                    , ("mint__isnull", "true")
                    ]
            getFilters availableFilters params `shouldMatchList`
                [ Right (FooCtxt ==. "foo")
                , Right (FooCdbl ==. 1.0)
                , Right (FooMint ==. Nothing)
                ]

        it "returns a list of mixed Right and Left if the parameters contain an invalid value" $ do
            let
                availableFilters =
                    [ ("ctxt", toFilterValue >=> (Just . (==.) FooCtxt))
                    , ("cdbl", toFilterValue >=> (Just . (==.) FooCdbl))
                    , ("mint__isnull", toFilterValue >=> (\b -> Just $ (if b then (==.) else (!=.)) FooMint Nothing))
                    ]
                params =
                    [ ("ctxt", "foo")
                    , ("cdbl", "foo")
                    , ("mint__isnull", "true")
                    ]
                result = getFilters availableFilters params
                (ls, rs) = partitionEithers result

            rs `shouldMatchList`
                [ FooCtxt ==. "foo"
                , FooMint ==. Nothing
                ]
            ls `shouldSatisfy` (1 ==) . length

    describe "getOrderBy" $ do
        context "Right cases" $ do
            let tests =
                    [ (Just $ Asc FooCtxt,  [("cint", Asc FooCint)],   Just "cint",  Right $ Just $ Asc FooCint)
                    , (Nothing,             [("cint", Asc FooCint)],   Just "cint",  Right $ Just $ Asc FooCint)
                    , (Just $ Asc FooCtxt,  [("cint", Asc FooCint)],   Nothing,      Right $ Just $ Asc FooCtxt)
                    , (Just $ Asc FooCtxt,  [],                        Nothing,      Right $ Just $ Asc FooCtxt)
                    , (Nothing,             [("cint", Asc FooCint)],   Nothing,      Right Nothing)
                    , (Just $ Desc FooCtxt, [("-cint", Desc FooCint)], Just "-cint", Right $ Just $ Desc FooCint)
                    , (Nothing,             [("-cint", Desc FooCint)], Just "-cint", Right $ Just $ Desc FooCint)
                    , (Just $ Desc FooCtxt, [("-cint", Desc FooCint)], Nothing,      Right $ Just $ Desc FooCtxt)
                    , (Just $ Desc FooCtxt, [],                        Nothing,      Right $ Just $ Desc FooCtxt)
                    , (Nothing,             [("-cint", Desc FooCint)], Nothing,      Right Nothing)
                    , (Nothing,             [],                        Nothing,      Right Nothing)
                    ]
            forM_ tests $ \(defaultOrderBy, availableOrderBys, msort, ret) ->
                it ("returns " ++ show ret ++ " for " ++ show defaultOrderBy ++ ", " ++ show availableOrderBys ++ ", " ++ show msort) $
                    getOrderBy defaultOrderBy availableOrderBys msort `shouldBe` ret

        context "Left cases" $ do
            let tests =
                    [ (Just $ Asc FooCtxt, [],                      Just "cint")
                    , (Nothing,            [],                      Just "cint")
                    , (Just $ Asc FooCtxt, [("ctxt", Asc FooCtxt)], Just "cint")
                    , (Nothing,            [("ctxt", Asc FooCtxt)], Just "cint")
                    ]
            forM_ tests $ \(defaultOrderBy, availableOrderBys, msort) ->
                it ("returns Left for " ++ show defaultOrderBy ++ ", " ++ show availableOrderBys ++ ", " ++ show msort) $
                    getOrderBy defaultOrderBy availableOrderBys msort `shouldSatisfy` isLeft

    describe "getOffsetBy" $ do
        context "Right cases" $ do
            let tests =
                    [ (OFFSET 0, Just "1", Right $ Just $ OffsetBy 1)
                    , (NoOffset, Just "1", Right $ Just $ OffsetBy 1)
                    , (OFFSET 0, Nothing,  Right $ Just $ OffsetBy 0)
                    , (NoOffset, Nothing,  Right Nothing)
                    ]
            forM_ tests $ \(defaultOffset', moffset, ret) ->
                it ("returns " ++ show ret ++ " for " ++ show defaultOffset' ++ ", " ++ show moffset) $
                    getOffsetBy defaultOffset' moffset `shouldBe` (ret :: Either Text (Maybe (SelectOpt Foo)))

        context "Left cases" $ do
            let tests =
                    [ (OFFSET 0, Just "foo")
                    , (NoOffset, Just "foo")
                    ]
            forM_ tests $ \(defaultOffset', moffset) ->
                it ("returns Left for " ++ show defaultOffset' ++ ", " ++ show moffset) $
                    (getOffsetBy defaultOffset' moffset :: Either Text (Maybe (SelectOpt Foo))) `shouldSatisfy` isLeft

    describe "getLimitTo" $ do
        context "Right cases" $ do
            let tests =
                    [ (LIMIT 0, Just "1", Right $ Just $ LimitTo 1)
                    , (NoLimit, Just "1", Right $ Just $ LimitTo 1)
                    , (LIMIT 0, Nothing,  Right $ Just $ LimitTo 0)
                    , (NoLimit, Nothing,  Right Nothing)
                    ]
            forM_ tests $ \(defaultLimit', mlimit, ret) ->
                it ("returns " ++ show ret ++ " for " ++ show defaultLimit' ++ ", " ++ show mlimit) $
                    getLimitTo defaultLimit' mlimit `shouldBe` (ret :: Either Text (Maybe (SelectOpt Foo)))

        context "Left cases" $ do
            let tests =
                    [ (LIMIT 0, Just "foo")
                    , (NoLimit, Just "foo")
                    ]
            forM_ tests $ \(defaultLimit', mlimit) ->
                it ("returns Left for " ++ show defaultLimit' ++ ", " ++ show mlimit) $
                    (getLimitTo defaultLimit' mlimit :: Either Text (Maybe (SelectOpt Foo))) `shouldSatisfy` isLeft


isALeft :: [Either Text (Filter record)] -> Bool
isALeft [x] = isLeft x
isALeft _   = False
