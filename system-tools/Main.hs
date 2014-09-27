
module Main where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import System.Environment


type PolyIx = Int
type System = Map PolyIx Poly
type Poly   = [Monom]
type Monom  = [Var]
type Var    = Int


-- Делинеаризация системы.
--  - избавляемся от промежуточных переменных;
--  - уравнения с большим числом переменных будут лучше поддаваться группировке;
--    - больше ядро группы => более компактные символы;
--  - FES быстрее перебирает меньшее число уравнений более высоких степеней 
main :: IO ()
main = getArgs >>= \case
  ["check", values] -> error "Not implemented"
  ["delinearize"] -> do
    eqs <- map read . lines <$> getContents
    let sys = Map.fromList $ zip [0..] $ reverse eqs
    let var :: Map Var [PolyIx]
        var = Map.fromListWith (\a b -> nub $ a ++ b)
              [(v,[k]) | (k,poly) <- Map.toList sys, v <- concat poly]
    sys' <- transform sys var Set.empty
    mapM_ print $ filter (not.null) $ Map.elems sys'
  _ -> error "Usage: system-tools (delinearize | check <vals>)"

-- TODO: gather candidates instead of banning

transform :: System -> Map Var [PolyIx] -> Set PolyIx -> IO System
transform sys varUsage badEqs = case getEq badEqs sys of
  Nothing -> return sys -- no more not banned matching eqs
  Just (k,v,eq) -> do
    let targetIxs = varUsage Map.! v
    let targetEqs = map (sys Map.!) targetIxs
    let transformedEqs = map (inline v eq) targetEqs

    -- Don't inline primary variables.
    -- Prevent building huge polynoms: limit degree, number of monoms and used
    -- variables.
    let ok =  v > 640
           && all (\p -> length p < 36 && polyDegree p < 10 && polyVars p < 17)
              transformedEqs
    if not ok
      then transform sys varUsage (Set.insert k badEqs)
      else do
        let changes = zipWith diff targetEqs transformedEqs
        let (oldVars, newVars) = unzip
              [ (map (,ix) old, map (,ix) new)
              | (ix, (old, new)) <- zip targetIxs changes
              ]
        let add (var,ix) = Map.adjust (ix:)       var
        let del (var,ix) = Map.adjust (delete ix) var
        let varUsage'
              = (\vu -> foldl' (flip add) vu $ concat newVars)
              $ (\vu -> foldl' (flip del) vu $ concat oldVars)
              $ varUsage
        let sys'
              = foldl' (flip $ uncurry Map.insert) sys
              $ zip targetIxs transformedEqs

        transform sys' varUsage' badEqs


polyDegree :: Poly -> Int
polyDegree [] = 0
polyDegree xs = maximum $ map length xs

polyVars :: Poly -> Int
polyVars = length . nub . concat


diff :: Poly -> Poly -> ([Var], [Var])
diff old new = (x \\ z, y \\ z)
  where
    x = nub $ concat old
    y = nub $ concat new
    z = intersect x y


getEq :: Set PolyIx -> System -> Maybe (PolyIx, Var, Poly)
getEq badEqs = loop . Map.toList
  where
    loop = \case
      (k, eq) : eqs
        | Set.member k badEqs -> loop eqs
        | otherwise
          -> case eq of
            [[v]]             -> Just (k, v, [])
            -- 0 = 1 + abcd => a = 1
            -- {b,c,d} = 1 will be inlined on next iterations
            [[], v:_]         -> Just (k, v, [[]])

            [[x],[a,b],[y]] | x == a && y == b         --  x \/  y = 0
                              -> Just (k, y, [])
            [[],[_,b],[y]]  | y == b                   -- -x \/  y = 0
                              -> Just (k, y, [])
            [[],[x],[a,y]]  | x == a                   --  x \/ -y = 0
                              -> Just (k, y, [[]])

            [[],[x],[v]]      -> Just (k, v, [[],[x]]) -- inline nots
            [[x],[v]]         -> Just (k, v, [[x]])    -- inline aliases
            -- inline ands
            [[v],xs]          -> Just (k, v, [xs])
            [xs,[v]]          -> Just (k, v, [xs])
            -- ad-hoc inlinings
            [    [a],[b],[v]] -> Just (k, v, [    [a],[b]])
            [[ ],[v],[b],[c]] -> Just (k, v, [[ ],[b],[c]])
            [[a],[v],[b],[c]] -> Just (k, v, [[a],[b],[c]])
            _ -> loop eqs
      [] -> Nothing


inline :: Var -> Poly -> Poly -> Poly
inline v inlPoly = simpl . concatMap (simplAnd [[]])
  where
    polyMul xs ys = concat [map (y++) xs | y <- ys]

    simplAnd []  _      = []  -- shortcut to zero
    simplAnd res []     = res
    simplAnd res (x:xs) = simplAnd
      (polyMul res $ if x == v then inlPoly else [[x]])
      xs

    simpl :: Poly -> Poly
    simpl
      = map head . filter (odd . length) . group . sort
      . map (nub . sort)
