module Language.Swift.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure)

import System.FilePath ((</>), takeExtension)
import System.Directory (getDirectoryContents)

import Control.Applicative ((<$>))
import Control.Monad (forM_)

import qualified Language.Swift as S

tests :: Test
tests = testGroup "Language.Swift.Tests"
    [ parseFileTest "***Running Specs***" "./tests/specs"
    ]

parseFileTest :: String -> FilePath -> Test
parseFileTest msg root = testCase msg $ do
  swiftFiles <- map (root </>) . filter ((==) ".swift" . takeExtension) <$> getDirectoryContents root
  putStrLn $ "Found " ++ show (length swiftFiles) ++ " Files"
  forM_ swiftFiles $ \file -> do
    putStrLn $ "Parsing File: " ++ file
    res <- S.parseFile file
    case res of
     Left err  -> assertFailure ("Error parsing file: " ++ show err)
     Right _ -> putStrLn "✓"
