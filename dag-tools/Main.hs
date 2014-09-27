

import Control.Applicative
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import System.Environment (getArgs)


main :: IO ()
main = do
  getArgs >>= \case
    ["eval-check", inBits, outBits] ->
      print =<< evalCheck
        <$> (parseBits <$> readFile inBits)
        <*> (parseBits <$> readFile outBits)
        <*> (map read . lines <$> getContents)
    ["eval-trace", inBits] -> return () -- print bits from evalCheck
    ["fix-inputs", inBits] ->
      mapM_ print =<< fixInputs
        <$> (parseBits <$> readFile inBits)
        <*> (map read . lines <$> getContents)
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
evalCheck inBits outBits = loop Map.empty
  where
    loop _ [] = True
    loop bits ((ref,ex):exs) = case ex of
      Inp i -> case Map.lookup i inBits of
        Nothing -> error $ "Input is not fixed " ++ show i
        Just v  -> loop (Map.insert ref v bits) exs
      Out o poly ->
        case Map.lookup o outBits of
        Nothing -> error $ "Output is not fixed " ++ show o
        Just a
          | a == evalBool bits poly -> loop bits exs
          | otherwise -> error $ "Output bit mismatch " ++ show o
      Node poly ->
        let v = evalBool bits poly
        in  loop (Map.insert ref v bits) exs



fixInputs :: IntMap Bool -> Dag -> Dag
fixInputs inBits = loop Map.empty
  where
    loop _ [] = []
    loop inlMap ((ref,ex):exs) = case ex of
      Inp i -> case Map.lookup i inBits of
        Nothing -> (ref,ex) : loop inlMap exs
        Just v  ->
          let v' = if v then [[]] else []
          in  loop (Map.insert ref v' inlMap) exs
      Out o poly -> (ref, Out o $ inline inlMap poly) : loop inlMap exs
      Node poly -> case inline inlMap poly of
        -- inline constants, aliases, negations
        []       -> loop (Map.insert ref [] inlMap) exs
        [[]]     -> loop (Map.insert ref [[]] inlMap) exs
        [[x]]    -> loop (Map.insert ref [[x]] inlMap) exs
        [[],[x]] -> loop (Map.insert ref [[],[x]] inlMap) exs
        poly'    -> (ref, Node poly') : loop inlMap exs


type Ref = Int
type Poly = [[Ref]]
data Exp
  = Inp Int
  | Out Int Poly
  | Node Poly
  deriving (Show, Read)

type Dag = [(Ref,Exp)]


evalBool :: IntMap Bool -> Poly -> Bool
evalBool bits = evalXor
  where
    evalXor = odd . length . filter evalAnd
    evalAnd = all value
    value r = case Map.lookup r bits of
      Nothing -> error $ "Inconsistent DAG. Unevaluated ref " ++ show r
      Just v  -> v


inline :: IntMap Poly -> Poly -> Poly
inline inlMap = simpl . evalXor
  where
    evalXor = concatMap (evalAnd [])
    evalAnd res [] = [res]
    evalAnd res (x:xs) = case Map.lookup x inlMap of
        Nothing -> evalAnd (x:res) xs
        Just ys -> evalXor $ map (++res++xs) ys


simpl :: Poly -> Poly
simpl
  = map head . filter (odd . length) . group . sort
  . map (nub . sort)


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
