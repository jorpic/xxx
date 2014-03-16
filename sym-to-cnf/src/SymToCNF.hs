
module Main where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Read as L
import Text.Printf
import Data.List.Split
import Data.Word
import Data.Bits ((.&.))
import Options.Applicative

import Types
import Tseitin


main :: IO ()
main = execParser (info (helper <*> options) fullDesc) >>= realMain


options :: Parser (Int,String)
options = (,)
  <$> option (long "total-vars")
  <*> strOption (long "method")


realMain :: (Int, String) -> IO ()
realMain (totalVars, method) = do
  ls <- L.lines <$> L.getContents
  let syms = map
        (\(vars, cubes) -> map (decodeCube vars) cubes)
        (parseSyms ls)
  L.putStr
    $ case method of
      "tseitin" -> uncurry dimacs $ tseitin totalVars syms
      _ -> error "Invalid method"


type EncodedCube = Word64
-- i-th bit represents whether i-th variable is negated or not in the cube

decodeCube :: [Var] -> EncodedCube -> Cube
decodeCube varMap w =
  [ if w .&. (2^i) == 0 then -l else l
  | (i,v) <- zip [0..63::Int] varMap
  , let l = fromIntegral v
  ]


parseSyms :: [Text] -> [([Var], [EncodedCube])]
parseSyms
  = map (\(hd:sym) -> (read $ L.unpack hd, map fromHex sym))
  . split (dropInitBlank $ keepDelimsL $ whenElt ("[" `L.isPrefixOf`))


fromHex :: Text -> EncodedCube
fromHex txt = case L.hexadecimal txt of
  Left err    -> error $ err ++ " at " ++ show txt
  Right (i,_) -> i


dimacs :: Int -> CNF -> Text
dimacs totalVars clauses
  = L.unlines $ cnfHeader : map tr clauses
  where
    cnfHeader = L.pack $ printf "p dnf %i %i" totalVars (length clauses)
    tr = \case
      Clause2 a b   -> L.pack $ unwords [show a, show b, "0"]
      Clause3 a b c -> L.pack $ unwords [show a, show b, show c, "0"]
      ClauseN xs    -> L.unwords $ map (L.pack . show) $ xs++[0]
