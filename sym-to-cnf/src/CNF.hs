
{-# OPTIONS_GHC -funbox-strict-fields #-}
module CNF where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import Text.Printf
import Data.Word (Word32)
import Data.Int (Int32)


type Var = Word32
type Lit = Int32  -- variable or its negation

data Clause -- Disjunction of literals
  = Clause2 !Lit !Lit
  | Clause3 !Lit !Lit !Lit
  | ClauseN [Lit]

type CNF = [Clause] -- Conjunction of clauses


dimacs :: Int -> CNF -> Text
dimacs totalVars clauses
  = L.unlines $ cnfHeader : map tr clauses
  where
    cnfHeader = L.pack $ printf "p cnf %i %i" totalVars (length clauses)
    tr = \case
      Clause2 a b   -> L.pack $ unwords [show a, show b, "0"]
      Clause3 a b c -> L.pack $ unwords [show a, show b, show c, "0"]
      ClauseN xs    -> L.unwords $ map (L.pack . show) $ xs++[0]
