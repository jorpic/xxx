
module BDD where

import Data.Int
import OBDD as BDD

import Symbol
import CNF

viaBDD :: Int -> [[Cube]] -> (Int, CNF)
viaBDD totalVars [symbol] = (convertSymbol symbol, [])


convertSymbol :: [Cube] -> Int
convertSymbol = BDD.size . BDD.or . map convertCube


convertCube :: Cube -> OBDD Int32
convertCube cube = BDD.and [BDD.unit (abs v) (v > 0) | v <- cube]

