module Yesod.Filter.Read where

import           Data.Char                (isSpace, toLower, toUpper)
import           Data.Time                (Day, TimeOfDay, UTCTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Text.Read                (readMaybe)


-- Parse a string to Int.
-- |
-- >>> readMaybeInt "1"
-- Just 1
--
-- >>> readMaybeInt "one"
-- Nothing
--
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe'

-- Parse a string to Double.
-- |
-- >>> readMaybeDouble "1.0"
-- Just 1.0
--
-- >>> readMaybeDouble "1"
-- Just 1.0
--
readMaybeDouble :: String -> Maybe Double
readMaybeDouble = readMaybe'

-- Parse a string to Bool.
-- |
-- >>> readMaybeBool "true"
-- Just True
--
-- >>> readMaybeBool "FALSE"
-- Just False
--
readMaybeBool :: String -> Maybe Bool
readMaybeBool = readMaybe' . capitalize

-- Parse a string to Day.
-- |
-- >>> readMaybeDay "2020-01-01"
-- Just 2020-01-01
--
-- >>> readMaybeDay "2020/01/01"
-- Nothing
--
readMaybeDay :: String -> Maybe Day
readMaybeDay = readMaybe'

-- Parse a string to TimeOfDay.
-- |
-- >>> readMaybeTimeOfDay "12:34:56"
-- Just 12:34:56
--
-- >>> readMaybeTimeOfDay "12:34:56.789"
-- Just 12:34:56.789
--
readMaybeTimeOfDay :: String -> Maybe TimeOfDay
readMaybeTimeOfDay = readMaybe'

-- Parse a string to UTCTime.
-- |
-- >>> readMaybeUTCTime "2020-01-01T12:34:56Z"
-- Just 2020-01-01 12:34:56 UTC
--
-- >>> readMaybeUTCTime "2020-01-01T12:34:56+09:00"
-- Nothing
--
readMaybeUTCTime :: String -> Maybe UTCTime
readMaybeUTCTime = iso8601ParseM

-- Wrapper function of `readMaybe` that returns Nothing if a string has leading/trailing whitespaces.
-- |
-- >>> readMaybe' "1" :: Maybe Int
-- Just 1
--
-- >>> readMaybe' " 1" :: Maybe Int
-- Nothing
--
-- >>> readMaybe' "1 " :: Maybe Int
-- Nothing
--
readMaybe' :: Read a => String -> Maybe a
readMaybe' "" = readMaybe ""
readMaybe' s
    | isSpace (head s) || isSpace (last s) = Nothing
    | otherwise                            = readMaybe s

-- Capitalize a string.
-- |
-- >>> capitalize "foo"
-- "Foo"
--
-- >>> capitalize "FOO"
-- "Foo"
--
capitalize :: String -> String
capitalize []       = []
capitalize (x : xs) = toUpper x : map toLower xs
