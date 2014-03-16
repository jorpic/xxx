
module Main where


import Control.Applicative
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Read as L

import System.Environment (getArgs)

import Symbol


main :: IO ()
main = getArgs >>= \case
  res:symbols -> do
    valuation <- parseRes <$> L.readFile res
    mapM (fmap (checkSym valuation) . L.readFile) symbols
      >>= print
  _ -> error "Invalid args"


parseRes :: Text -> IntMap Int
parseRes res = case L.lines res of
  ["SAT", vars] -> Map.fromList
    [(abs x, x)
    | v <- L.words vars
    , let Right (x,_)  = L.signed L.decimal v
    ]
  _ -> error "UNSAT or invalid format"


checkSym :: IntMap Int -> Text -> Bool
checkSym vals txt = any (==res) cubes
  where
    Just (vars,cubes) = parseSymbol $ L.lines txt
    res = map (fromIntegral . (vals Map.!) . fromIntegral) vars
