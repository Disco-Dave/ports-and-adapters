module Main (main) where

import Spec (spec)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize)


addTimeout :: IO () -> IO ()
addTimeout specItem = do
  result <- timeout 10_000_000 specItem

  case result of
    Nothing -> fail "Spec item took longer than 10 seconds."
    Just () -> pure ()


main :: IO ()
main =
  hspec
    . modifyMaxSize (const 100)
    . parallel
    $ around_ addTimeout spec
