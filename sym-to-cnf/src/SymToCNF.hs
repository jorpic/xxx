
module Main where

import Options.Applicative
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Symbol (parseSymbol)
import CNF
import BDD (bddTseitin)


main :: IO ()
main = execParser (info (helper <*> options) fullDesc)
  >>= realMain

options :: Parser (Var, String, [String])
options = (,,)
  <$> option (long "max-var")
  <*> strOption (long "method")
  <*> many (argument Just idm)


realMain :: (Var, String, [String]) -> IO ()
realMain (maxVar, method, symFiles) = do
  symData <- mapM (fmap (parseSymbol . L.lines) . L.readFile) symFiles
  let Just (_,syms) = unzip <$> sequence symData
  L.putStr
    $ case method of
      "bdd"     -> uncurry dimacs $ bddTseitin maxVar syms
      _ -> error "Invalid method"
