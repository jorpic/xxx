
{-# OPTIONS_GHC -funbox-strict-fields #-}
module CNF where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import Text.Printf
import Data.Word (Word32)


type Var = Word32
type Lit = Int      -- variable or its negation
type Clause = [Lit] -- Disjunction of literals
type CNF = [Clause] -- Conjunction of clauses

var2lit :: Var -> Lit
var2lit = fromIntegral

lit2var :: Lit -> Var
lit2var = fromIntegral . abs


dimacs :: CNF -> Var -> Text
dimacs clauses totalVars
  = L.unlines $ cnfHeader : map tr clauses
  where
    cnfHeader = L.pack $ printf "p cnf %i %i" totalVars (length clauses)
    tr xs = L.unwords $ map (L.pack . show) $ xs++[0]
