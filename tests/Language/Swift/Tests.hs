module Language.Swift.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, once)
import Test.QuickCheck.Monadic (monadic, run, assert)
import Test.QuickCheck.Property (morallyDubiousIOProperty)

tests :: Test
tests = testGroup "Language.Swift.Tests"
    [ testProperty "Let-bindings" testConstantDeclarations
    ]

testConstantDeclarations :: Property
testConstantDeclarations = undefined

