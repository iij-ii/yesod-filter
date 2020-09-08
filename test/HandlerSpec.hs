{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HandlerSpec (spec) where

import           Data.Time       (TimeOfDay (TimeOfDay), timeOfDayToTime)

import           TestApplication


spec :: Spec
spec = withApp $ do
    describe "Basic cases" $ do
        it "returns 200 when no query string is specified" $ do
            setupDB foos
            get FoosR
            statusIs 200
            (res :: [Foo]) <- requireJSONResponse
            liftIO $ res `shouldBe` foos

        it "returns 400 when an invalid field is specified" $ do
            setupDB foos
            request $ do
                addGetParam "unknown" "0"
                setUrl FoosR
            statusIs 400

    describe "Filtering cases" $ do
        context "OK" $ do
            let tests =
                    [ ("cint", "2", filter (\f -> fooCint f == 2) foos)
                    , ("cint__ne", "2", filter (\f -> fooCint f /= 2) foos)
                    , ("cint__gt", "2", filter (\f -> fooCint f > 2) foos)
                    , ("cint__lt", "2", filter (\f -> fooCint f < 2) foos)
                    , ("cint__ge", "2", filter (\f -> fooCint f >= 2) foos)
                    , ("cint__le", "2", filter (\f -> fooCint f <= 2) foos)
                    ]
            forM_ tests $ \(param, val, ret) ->
                it ("returns 200 for " ++ unpack param ++ "=" ++ unpack val) $ do
                    setupDB foos
                    request $ do
                        addGetParam param val
                        setUrl FoosR
                    statusIs 200
                    (res :: [Foo]) <- requireJSONResponse
                    liftIO $ res `shouldBe` ret

        context "Bad Request" $ do
            let tests =
                    [ ("cdbl", "0.0")       -- not specified in filterDefs
                    , ("ctxt__ne", "foo")   -- not specified in filterDefs
                    , ("cint", "NaN")       -- invalid type
                    ]
            forM_ tests $ \(param, val) ->
                it ("returns 400 for " ++ unpack param ++ "=" ++ unpack val) $ do
                    setupDB foos
                    request $ do
                        addGetParam param val
                        setUrl FoosR
                    statusIs 400

    describe "Sorting cases" $ do
        context "OK" $ do
            let tests =
                    [ ("sort", "cint", sortOn fooCint foos)
                    , ("sort", "-cint", sortOn (Down . fooCint) foos)
                    ]
            forM_ tests $ \(param, val, ret) ->
                it ("returns 200 for " ++ unpack param ++ "=" ++ unpack val) $ do
                    setupDB foos
                    request $ do
                        addGetParam param val
                        setUrl FoosR
                    statusIs 200
                    (res :: [Foo]) <- requireJSONResponse
                    liftIO $ res `shouldBe` ret

        context "Bad Request" $ do
            let tests =
                    [ ("sort", "unknown")
                    , ("sort", "ctxt")  -- not specified in sortFields
                    ]
            forM_ tests $ \(param, val) ->
                it ("returns 400 for " ++ unpack param ++ "=" ++ unpack val) $ do
                    setupDB foos
                    request $ do
                        addGetParam param val
                        setUrl FoosR
                    statusIs 400

    describe "Pagination cases" $ do
        context "OK" $ do
            let tests =
                    [ ("offset", "1", drop 1 foos)
                    , ("limit", "2", take 2 foos)
                    ]
            forM_ tests $ \(param, val, ret) ->
                it ("returns 200 for " ++ unpack param ++ "=" ++ unpack val) $ do
                    setupDB foos
                    request $ do
                        addGetParam param val
                        setUrl FoosR
                    statusIs 200
                    (res :: [Foo]) <- requireJSONResponse
                    liftIO $ res `shouldBe` ret

        context "Bad Request" $ do
            let tests =
                    [ ("offset", "one")
                    , ("limit", "two")
                    ]
            forM_ tests $ \(param, val) ->
                it ("returns 400 for " ++ unpack param ++ "=" ++ unpack val) $ do
                    setupDB foos
                    request $ do
                        addGetParam param val
                        setUrl FoosR
                    statusIs 400

setupDB :: [Foo] -> SIO (YesodExampleData App) ()
setupDB records = do
    _ <- runDB $ deleteWhere ([] :: [Filter Foo])
    _ <- runDB $ insertMany records
    pure ()

foos :: [Foo]
foos =
    [ Foo
        "One"
        1
        1.0
        True
        (fromGregorian 2020 1 1)
        (TimeOfDay 12 34 56)
        (UTCTime (fromGregorian 2020 1 1) (timeOfDayToTime $ TimeOfDay 12 34 56))
        (Just "One")
        (Just 1)
        (Just 1.0)
        (Just True)
        (Just (fromGregorian 2020 1 1))
        (Just (TimeOfDay 12 34 56))
        (Just (UTCTime (fromGregorian 2020 1 1) (timeOfDayToTime $ TimeOfDay 12 34 56)))
    , Foo
        "Two"
        2
        2.0
        True
        (fromGregorian 2020 1 2)
        (TimeOfDay 12 34 57)
        (UTCTime (fromGregorian 2020 1 2) (timeOfDayToTime $ TimeOfDay 12 34 57))
        (Just "One")
        (Just 2)
        (Just 2.0)
        (Just True)
        (Just (fromGregorian 2020 1 2))
        (Just (TimeOfDay 12 34 57))
        (Just (UTCTime (fromGregorian 2020 1 2) (timeOfDayToTime $ TimeOfDay 12 34 57)))
    , Foo
        "Three"
        3
        3.0
        False
        (fromGregorian 2020 1 3)
        (TimeOfDay 12 34 58)
        (UTCTime (fromGregorian 2020 1 3) (timeOfDayToTime $ TimeOfDay 12 34 58))
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
    ]
