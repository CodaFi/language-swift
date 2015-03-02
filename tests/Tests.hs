module Main where

import Test.Framework (defaultMain)
import Language.Swift.Tests (tests)

main :: IO ()
main = defaultMain
    [ tests 
    ]
