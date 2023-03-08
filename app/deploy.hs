module Main where

import Control.Exception (throwIO)
import Data.List
import qualified Test
import System.Environment (getArgs)

--Modulo:

main :: IO ()
main = do
  Test.evaluate
