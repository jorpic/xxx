

import Control.Applicative
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import System.Environment (getArgs)

-- FIXME: reverse DAG

main :: IO ()
main = do
  dag <- map read . lines <$> getContents
  getArgs >>= \case
    ["eval-check", inBits, outBits] -> do
      print =<< evalCheck
        <$> (parseBits <$> readFile inBits)
        <*> (parseBits <$> readFile outBits)
        <*> pure dag
    ["eval-trace", inBits] -> return () -- print bits from evalCheck
    ["fix-inputs", inBits] -> do
      mapM_ print =<< evalCheck
    ["fix-outputs", outBits] -> return ()
    ["to-system"] -> return ()
    _ -> error
      $  "Usage: dag-tools <command>\n\
         \  Commands:\n\
         \    eval-check  <in-bits> <out-bits>\n\
         \    eval-trace  <in-bits>\n\
         \    fix-inputs  <in-bits>\n\
         \    fix-outputs <out-bits>\n\
         \    to-system"


evalCheck :: IntMap Bool -> IntMap Bool -> Dag -> Bool
evalCheck inBits outBits = loop Map.empty . reverse
  where
    loop bits [] = True
    loop bits ((ref,ex):exs) = case ex of
      Inp i -> case Map.lookup i inBits of
        Nothing -> error $ "Input bit not found " ++ show i
        Just v  -> loop (Map.insert ref v bits) exs
      Out o r -> case Map.lookup o outBits of
        Nothing -> error $ "Output bit not found " ++ show o
        Just a  -> case Map.lookup r bits of
          Nothing -> error $ "Inconsistent DAG. Unevaluated ref " ++ show r
          Just b
            | a == b -> loop bits exs
            | otherwise -> error $ "Check failed on output bit " ++ show r
      Node poly ->
        let v = evalBool bits poly
        in loop (Map.insert ref v bits) exs


type Ref = Int
data Exp
  = Inp Int
  | Out Int Ref
  | Node [[Ref]]
  deriving (Show, Read)

type Dag = [(Ref,Exp)]


evalBool :: IntMap Bool -> [[Ref]] -> Bool
evalBool bits = evalXor
  where
    evalXor = odd . length . filter evalAnd
    evalAnd = all value
    value r = case Map.lookup r bits of
      Nothing -> error $ "Inconsistent DAG. Unevaluated ref " ++ show r
      Just v  -> v


parseBits :: String -> IntMap Bool
parseBits
  = Map.fromList
  . concatMap (\case
      (i,'0') -> [(i,False)]
      (i,'1') -> [(i,True)]
      _ -> [])
  . zip [0..]
  . concat . concatMap words
  . lines


simpl :: [[Ref]] -> [[Ref]]
simpl
  = map head . filter (odd . length) . group . sort
  . map (nub . sort)




