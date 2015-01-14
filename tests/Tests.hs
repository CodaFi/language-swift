module Main where

import Test.Framework (defaultMain)

import qualified Language.Swift.Tests

main :: IO ()
main = defaultMain
    [ Language.Swift.Tests.tests 
    ]
