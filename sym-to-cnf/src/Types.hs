
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Types where

import Data.Word (Word32)
import Data.Int (Int32)


type Var = Word32
type Lit = Int32    -- variable or its negation
type Cube = [Lit]   -- Conjunction of literals

data Clause -- Disjunction of literals
  = Clause2 !Lit !Lit
  | Clause3 !Lit !Lit !Lit
  | ClauseN [Lit]

type CNF = [Clause] -- Conjunction of clauses
