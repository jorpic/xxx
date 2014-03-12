
module Types where

import Data.Word (Word32)
import Data.Int (Int32)

type Var = Word32
type Lit = Int32    -- variable or its negation
type Cube = [Lit]   -- Conjunction of literals
type Clause = [Lit] -- Disjunction of literals
type CNF = [Clause] -- Conjunction of clauses
