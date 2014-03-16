
module Main where

import Options.Applicative
import qualified Data.Text.Lazy.IO as L

import Symbol (parseSymbols)
import CNF (dimacs)
import Tseitin (tseitin)


main :: IO ()
main = execParser (info (helper <*> options) fullDesc) >>= realMain


options :: Parser (Int,String)
options = (,)
  <$> option (long "total-vars")
  <*> strOption (long "method")


realMain :: (Int, String) -> IO ()
realMain (totalVars, method) = do
  Just (_,syms) <- fmap unzip . parseSymbols <$> L.getContents
  L.putStr
    $ case method of
      "tseitin" -> uncurry dimacs $ tseitin totalVars syms
      _ -> error "Invalid method"
