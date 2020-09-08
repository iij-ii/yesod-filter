-- {-# OPTIONS_GHC -F -pgmF doctest-driver -optF config.json #-}
import           Test.DocTest

main :: IO ()
main = doctest
    [ "-isrc"
    , "src/Yesod/Filter/Read.hs"
    ]
