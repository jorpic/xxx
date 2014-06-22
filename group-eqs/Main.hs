
module Main where


import Control.Applicative
import System.Random
import qualified Data.IntSet as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List
import Data.Ord (comparing)
import System.IO


type Var = Int
type Equ = [[Var]]
type Group = [Equ]


main :: IO ()
main = do
  sys <- map read . lines <$> getContents
  let varUsage = countVarUsage sys
  groupEqs varUsage sys


groupEqs :: IntMap Int -> [Equ] -> IO ()
groupEqs varUsage
  -- Сначала выбираем уравнения содержащие ключевые переменные.
  -- Передположительно, это позволит получить для них более качественные
  -- группы.
  = loop 0 . sortBy (comparing concat)
  where
    loop _ []  = return ()
    loop i [e] = saveGroup varUsage i [e]
    loop i sys = do
      (seed,sys') <- extractRandom 256 sys
      let (grp, sys'') = growGroup varUsage [seed] sys'
      saveGroup varUsage i grp
      loop (i+1) sys''


growGroup :: IntMap Int -> Group -> [Equ] -> (Group, [Equ])
growGroup globalVarUsage grp sys
  = case candidates of
    [] -> (grp,sys)
    (eq,_):_ -> growGroup globalVarUsage (eq:grp) (delete eq sys)
  where
    grpVars = Set.fromList $ concatMap concat grp
    -- Чем меньше новых переменных добавляется к группе и чем больше общих
    -- переменных у группы и уравнения — тем лучше.
    candidates = sortBy (comparing snd)
      [(eq, (Set.size eqVars - commonVars, -commonVars))
      | eq <- sys
      , let grp'       = eq : grp
      , let grpVars'   = Set.size $ Set.fromList $ concatMap concat grp'
      , let eqVars     = Set.fromList $ concat eq
      , let commonVars = Set.size $ Set.intersection eqVars grpVars
      , let freeEqs    = filter (not.null.fst) $ extractFreeVars grp'
      , let grpVarUsage= countVarUsage grp'
      , let elimVars   = eliminatableVars globalVarUsage grpVarUsage
      , commonVars > 0
      , grpVars' - length elimVars < 65
      , grpVars' - length freeEqs < 28 -- 2^27 * 8 = 1Gb
      ]


extractFreeVars :: Group -> [([Var], Equ)]
extractFreeVars grp = map (go  ([],[])) grp
  where
    grpVarUsage = countVarUsage grp
    go (vs,terms) = \case
      [x]:xs | grpVarUsage Map.! x == 1 -> go (x:vs,terms) xs
      x:xs -> go (vs,x:terms) xs
      [] -> (vs,terms)


eliminatableVars :: IntMap Int -> IntMap Int -> [Var]
eliminatableVars globalVarUsage grpVarUsage
  = [v
    |v <- Set.toList $ Map.keysSet grpVarUsage
    ,v > 640
    ,Map.findWithDefault 0 v grpVarUsage == globalVarUsage Map.! v
    ]


-- we also count multiple usages in single eq
countVarUsage :: [Equ] -> IntMap Int
countVarUsage = foldl' addEqVars Map.empty
  where
    addEqVars m = foldr (\v -> Map.insertWith' (+) v 1) m . concat


saveGroup :: IntMap Int -> Int -> Group -> IO ()
saveGroup globalVarUsage i grp = do
  let grpVars = Set.fromList $ concatMap concat grp
  let grpVarUsage = countVarUsage grp
  let elimVars = eliminatableVars globalVarUsage grpVarUsage
  let grp' = extractFreeVars grp
  let freeEqs = length $ filter (not.null.fst) grp'
  withFile (show i ++ ".grp") WriteMode $ \h -> do
    hPutStrLn h $ "=== eqs " ++ show (length grp)
    hPutStrLn h $ "=== vars " ++ show (Set.size grpVars)
    hPutStrLn h $ "=== free " ++ show freeEqs
    hPutStrLn h $ "=== elimSize " ++  show (length elimVars)
    hPutStrLn h $ "=== elim "   ++  show elimVars
    mapM_ (hPrint h) $ sort grp'


-- extracthPrandom element form list
extractRandom :: Int -> [a] -> IO (a, [a])
extractRandom n xs = do
  i <- randomRIO (0, min (length xs) n - 1)
  let (ys, z:zs) = splitAt i xs
  return (z, ys ++ zs)
