

import Control.Applicative
import Data.List
import qualified Data.IntSet as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import System.Environment (getArgs)


main :: IO ()
main = do
  getArgs >>= \case
    ["eval-check", inBits, outBits] ->
      (mapM_ print . Map.toAscList) =<< evalCheck
        <$> (parseBits <$> readFile inBits)
        <*> (parseBits <$> readFile outBits)
        <*> (map read . lines <$> getContents)
    ["fix-inputs", inBits] ->
      mapM_ print =<< fixInputs
        <$> (parseBits <$> readFile inBits)
        <*> (map read . lines <$> getContents)
    ["to-system", outBits] ->
      mapM_ print =<< fixOutputs
        <$> (parseBits <$> readFile outBits)
        <*> (map read . lines <$> getContents)
    _ -> error
      $  "Usage: dag-tools <command>\n\
         \  Commands:\n\
         \    eval-check  <in-bits> <out-bits>\n\
         \    fix-inputs  <in-bits>\n\
         \    to-system <out-bits>"


evalCheck :: IntMap Bool -> IntMap Bool -> Dag -> IntMap Bool
evalCheck inBits outBits = loop Map.empty
  where
    loop bits [] = bits
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


fixOutputs :: IntMap Bool -> Dag -> System
fixOutputs outBits = loop [] Set.empty Map.empty . reverse
  where
    loop res _ _ [] = res
    loop res used inlMap ((ref,ex):exs) = case ex of
      Inp _      -> loop res used inlMap exs
      Out o poly -> case Map.lookup o outBits of
        Nothing -> loop res used inlMap exs -- skip don't care outputs
        Just v  -> loop' $ inline inlMap (if v then []:poly else poly)
      Node poly
        | not $ Set.member ref used -> loop res used inlMap exs -- skip unused refs
        | otherwise                 -> loop' $ inline inlMap ([ref] : poly)
      where
        -- FIXME: it is enough to do straightforward conversion here
        -- all simplifications will be done in system-tools
        loop' = \case
          []           -> error "Truism inferred" -- should we fail?
          [[]]         -> error "Conflict inferred"
          [[x]]        -> loop'' (use [x])   (Map.insert x [] inlMap)
          [[],[x]]     -> loop'' (use [x])   (Map.insert x [[]] inlMap) -- generalize to xs
          [[x],[y]]    -> loop'' (use [x,y]) (Map.insert y [[x]] inlMap)
          [[],[x],[y]] -> loop'' (use [x,y]) (Map.insert y [[],[x]] inlMap)
          -- TODO : x \/ y = 0, -x \/  y = 0,  x \/ -y = 0
          eq           -> loop (eq:res) (use $ concat eq) inlMap exs

        use = foldr Set.insert used

        -- new facts can affect already converted equations
        -- so we are trying to simplify them again
        loop'' used' inlMap' = loop (map (inline inlMap') res) used' inlMap' exs



type Ref = Int
type Poly = [[Ref]]
type System = [Poly]
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
