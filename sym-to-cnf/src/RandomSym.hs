
module Main where

import Numeric (showHex)
import Control.Monad (forM_, when)
import Data.List (nub, sort)
import Options.Applicative
import System.Random.MWC


options :: Parser (Int,Int,Int)
options = (,,)
  <$> option (long "total-vars")
  <*> option (long "vars-in-sym")
  <*> option (long "sym-size")


randomSym :: (Int,Int,Int) -> IO ()
randomSym (total,used,size) = do
  when (used > 32)
    $ error "vars-in-sym is > 32"

  withSystemRandom . asGenIO $ \gen -> do
    usedVars <- sequence $ replicate used $ uniformR (1,total) gen
    print $ nub $ sort usedVars
    forM_ [1..size] $ \_ -> do
      row <- uniformR (1,2^used) gen
      putStrLn $ '0' : showHex (row :: Int) ""


main :: IO ()
main = execParser (info (helper <*> options) fullDesc) >>= randomSym
