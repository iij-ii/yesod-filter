#!/usr/bin/env runhaskell
{-
Usage: geninst.hs <<EOF
Foo json
    ctxt Text
    cint Int
    mdbl Double Maybe
EOF
-}

import           Control.Monad (forM_)
import           Data.Char     (toUpper)


main :: IO ()
main = do
    contents <- getContents
    let defs = lines contents
    let entity = head . words $ head defs
    let fields = map (head . words) $ tail defs
    printInstances entity ("Id" : fields)

printInstances :: String -> [String] -> IO ()
printInstances entity fields = do
    let entityFields = map (\f -> entity ++ (toUpper (head f) : tail f)) fields
    putStrLn ""

    putStrLn $ "instance Eq (Filter " ++ entity ++ ") where"
    forM_ entityFields $ \ef ->
        putStrLn $ "    (==)  (Filter " ++ ef ++ " v1 f1) (Filter " ++ ef ++ " v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)"
    putStrLn "    (==) _ _ = undefined"
    putStrLn ""

    putStrLn $ "instance Show (Filter " ++ entity ++ ") where"
    forM_ entityFields $ \ef ->
        putStrLn $ "    show (Filter " ++ ef ++ " v f) = \"Filter " ++ ef ++ " (\" ++ show (FV v) ++ \") \" ++ show f"
    putStrLn "    show _ = undefined"
    putStrLn ""

    putStrLn $ "instance Eq (SelectOpt " ++ entity ++ ") where"
    forM_ entityFields $ \ef -> do
        putStrLn $ "    (==) (Asc " ++ ef ++ ") (Asc " ++ ef ++ ") = True"
        putStrLn $ "    (==) (Desc " ++ ef ++ ") (Desc " ++ ef ++ ") = True"
    putStrLn "    (==) (OffsetBy n1) (OffsetBy n2) = n1 == n2"
    putStrLn "    (==) (LimitTo n1) (LimitTo n2) = n1 == n2"
    putStrLn "    (==) _ _ = False"
    putStrLn ""

    putStrLn $ "instance Show (SelectOpt " ++ entity ++ ") where"
    forM_ entityFields $ \ef -> do
        putStrLn $ "    show (Asc " ++ ef ++ ") = \"Asc " ++ ef ++ "\""
        putStrLn $ "    show (Desc " ++ ef ++ ") = \"Desc " ++ ef ++ "\""
    putStrLn "    show (OffsetBy n) = \"OffsetBy \" ++ show n"
    putStrLn "    show (LimitTo n) = \"LimitTo \" ++ show n"
