
module Symbol (parseSymbol, parseSymbols, Cube, Symbol) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Read as L
import Data.List.Split
import Data.Int
import Data.Word
import Data.Bits ((.&.))


type Lit = Int32
type Var = Word32

type Cube = [Lit]   -- Conjunction of literals
type Symbol = ([Var], [Cube])


parseSymbols :: Text -> Maybe [Symbol]
parseSymbols
  = sequence
  . map parseSymbol
  . split (dropInitBlank $ keepDelimsL $ whenElt ("[" `L.isPrefixOf`))
  . L.lines


parseSymbol :: [Text] -> Maybe Symbol
parseSymbol [] = Nothing
parseSymbol (vars:cubes) = Just (vs, map (decodeCube vs) cs)
  where
    (vs, cs) = (read $ L.unpack vars, map fromHex cubes)


fromHex :: Text -> EncodedCube
fromHex txt = case L.hexadecimal txt of
  Left err    -> error $ err ++ " at " ++ show txt
  Right (i,_) -> i


type EncodedCube = Word64
-- i-th bit represents whether i-th variable is negated or not in the cube

decodeCube :: [Var] -> EncodedCube -> Cube
decodeCube varMap w =
  [ if w .&. (2^i) == 0 then -l else l
  | (i,v) <- zip [0..63::Int] varMap
  , let l = fromIntegral v
  ]
