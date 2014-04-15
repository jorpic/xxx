
module BDD where

import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Lazy
import Data.Functor.Identity
import OBDD as BDD

import Symbol
import CNF


type M r = WriterT CNF (StateT Var Identity) r


nextVar :: M Var
nextVar = lift $ do
  v <- get
  put $ v+1
  return $ v+1


bddTseitin :: Var -> [[Cube]] -> (CNF, Var)
bddTseitin maxVar symbols
  = runIdentity
  $ (`runStateT` maxVar)
  $ execWriterT
  $ mapM_ symbolToCNF symbols


symbolToCNF :: [Cube] -> M ()
symbolToCNF sym = do
  top <- BDD.foldM
    (\case
      True  -> return []   -- AND [] = 1
      False -> return [[]] -- OR  [] = 0
    )
    (\v l r -> do
      let c = var2lit v
      case (l,r) of
        ([]  ,[[]]) -> return [[-c]]
        ([[]],[])   -> return [[c]]
        _ -> do
          x <- var2lit <$> nextVar
          -- x => ITE c t f
          -- ≡ x => (c ∧ t) ∨ (-c ∧ f)
          -- ≡ (-c ∨ t ∨ -x) ∧ (c ∨ f ∨ -x)
          -- And (f ∨ t ∨ -x) for better implicativity
          tell $ case (l,r) of
            ([[f]],[])    -> [                   [c,f,-x]]
            ([[f]],[[]])  -> [[f,  -x],[-c,  -x],[c,f,-x]]
            ([[f]],[[t]]) -> [[f,t,-x],[-c,t,-x],[c,f,-x]]
            ([[]], [[t]]) -> [[  t,-x],[-c,t,-x],[c,  -x]]
            ([],   [[t]]) -> [         [-c,t,-x]         ]
            _ -> error $ "Malformed BDD: " ++ show (v,l,r)
          return [[x]]
    )
    (symbolToBDD sym)
  tell top


symbolToBDD :: [Cube] -> OBDD Var
symbolToBDD = BDD.or . map cubeToBDD

cubeToBDD :: Cube -> OBDD Var
cubeToBDD cube = BDD.and [BDD.unit (lit2var l) (l > 0) | l <- cube]
