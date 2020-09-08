{-# LANGUAGE OverloadedStrings #-}

module Yesod.Filter.ReadSpec (spec) where

import           Control.Monad     (forM_)
import           Data.Char         (isAlpha, isAscii, isLower, isUpper, toLower)
import           Data.Time         (TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian,
                                    timeOfDayToTime)
import           Test.Hspec
import           Test.QuickCheck

import           Yesod.Filter.Read


spec :: Spec
spec = do
    describe "readMaybeInt" $ do
        let tests =
                [ ("1", Just 1)
                , ("-1", Just (-1))
                , ("01", Just 1)
                , ("0x0a", Just 10)
                , ("1.0", Nothing)
                , (" 1", Nothing)
                , ("1 ", Nothing)
                , ("", Nothing)
                ]
        forM_ tests $ \(arg, ret) ->
            it ("returns " ++ show ret ++ " for " ++ show arg) $
                readMaybeInt arg `shouldBe` ret

    describe "readMaybeDouble" $ do
        let tests =
                [ ("1.0", Just 1.0)
                , ("-1.0", Just (-1.0))
                , ("1", Just 1.0)
                , ("01", Just 1.0)
                , ("0x0a", Just 10.0)
                , (" 1.0", Nothing)
                , ("1.0 ", Nothing)
                , (".1", Nothing)
                , ("", Nothing)
                ]
        forM_ tests $ \(arg, ret) ->
            it ("returns " ++ show ret ++ " for " ++ show arg) $
                readMaybeDouble arg `shouldBe` ret

    describe "readMaybeBool" $ do
        let tests =
                [ ("True", Just True)
                , ("true", Just True)
                , ("TRUE", Just True)
                , ("False", Just False)
                , ("fALSE", Just False)
                , ("Tru", Nothing)
                , (" True", Nothing)
                , ("True ", Nothing)
                , ("0", Nothing)
                , ("", Nothing)
                ]
        forM_ tests $ \(arg, ret) ->
            it ("returns " ++ show ret ++ " for " ++ show arg) $
                readMaybeBool arg `shouldBe` ret

    describe "readMaybeDay" $ do
        let tests =
                [ ("2020-01-01", Just $ fromGregorian 2020 1 1)
                , ("2020/01/01", Nothing)
                , ("2020-04-31", Nothing)
                , ("January 1st 2020", Nothing)
                , (" 2020-01-01", Nothing)
                , ("2020-01-01 ", Nothing)
                , ("", Nothing)
                ]
        forM_ tests $ \(arg, ret) ->
            it ("returns " ++ show ret ++ " for " ++ show arg) $
                readMaybeDay arg `shouldBe` ret

    describe "readMaybeTimeOfDay" $ do
        let tests =
                [ ("12:34:56", Just $ TimeOfDay 12 34 56)
                , ("12:34:56.7", Just $ TimeOfDay 12 34 56.7)
                , ("12:34:56.789", Just $ TimeOfDay 12 34 56.789)
                , ("12:34", Nothing)
                , ("24:34:56", Nothing)
                , (" 12:34:56", Nothing)
                , ("12:34:56 ", Nothing)
                , ("", Nothing)
                ]
        forM_ tests $ \(arg, ret) ->
            it ("returns " ++ show ret ++ " for " ++ show arg) $
                readMaybeTimeOfDay arg `shouldBe` ret

    describe "readMaybeUTCTime" $ do
        let day = fromGregorian 2020 1 1
        let tests =
                [ ("2020-01-01T12:34:56Z", Just $ UTCTime day (timeOfDayToTime $ TimeOfDay 12 34 56))
                , ("2020-01-01T12:34:56.789Z", Just $ UTCTime day (timeOfDayToTime $ TimeOfDay 12 34 56.789))
                , ("2020-01-01T12:34:56+09:00", Nothing)
                , ("2020-1-1 12:34:56Z", Nothing)
                , ("2020-1-1T12:34:56Z", Nothing)
                , (" 2020-01-01T12:34:56Z", Nothing)
                , ("2020-01-01T12:34:56Z ", Nothing)
                , ("", Nothing)
                ]
        forM_ tests $ \(arg, ret) ->
            it ("returns " ++ show ret ++ " for " ++ show arg) $
                readMaybeUTCTime arg `shouldBe` ret

    describe "capitalize" $ do
        let tests =
                [ ("foo", "Foo")
                , ("BAR", "Bar")
                , (" BAZ", " baz")
                , ("", "")
                ]
        forM_ tests $ \(arg, ret) ->
            it ("returns " ++ show ret ++ " for " ++ show arg) $
                capitalize arg `shouldBe` ret

        context "when used with strings" $
            it "returns a string with the first character converted to uppercase and the rest to lowercase" $
                property prop_Capitalize

prop_Capitalize :: String -> Bool
prop_Capitalize s = case s of
    [] -> null cs
    _  -> (map toLower s == map toLower cs) && isUpper' (head cs) && all isLower' (tail cs)
  where
    cs = capitalize s
    isUpper' c
        | isAscii c && isAlpha c = isUpper c
        | otherwise = True
    isLower' c
        | isAscii c && isAlpha c = isLower c
        | otherwise = True
