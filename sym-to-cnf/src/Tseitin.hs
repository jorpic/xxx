
module Tseitin where


import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Bits
import Data.Word

import Types


data St = St
  {nextVar  :: Var
  ,varCache :: IntMap Var
    -- cache intermediate variable for each two-literal conjunction
  }


tseitin :: Int -> [Cube] -> CNF
tseitin totalVars cubes
  = (`evalState` St (fromIntegral $ totalVars+1) Map.empty)
  $ do
    (vars, clauses) <- unzip <$> mapM ts cubes
    return $ vars : concat clauses
  where
    ts (x:xs) = loop [] x xs
    ts _ = error "BUG"

    loop :: CNF -> Lit -> Cube -> State St (Lit, CNF)
    loop res x [] = return (x, res)
    loop res x (y:ys) = do
      newVar x y >>= \case
        Left  z -> loop res (fromIntegral z) ys -- cache hit, old variable
        Right z' -> do -- cache miss, new variable
          let z  = fromIntegral z'
              eq = [[-z,x],[-z,y],[z,-x,-y]] -- CNF(z<=>x&y)
          loop (eq ++ res) z ys


newVar :: Lit -> Lit -> State St (Either Var Var)
newVar x y = do
  -- we encode pair of literals as single 64-bit word
  -- to use to as a key in IntMap
  let a = fromIntegral $ min x y :: Word64
      b = fromIntegral $ max x y :: Word64
      ab = (a `shift` 32) .|. (b .&. 0xffffffff) :: Word64
      key = fromIntegral ab :: Int
  St{..} <- get
  -- NB: we silently assume 64-bit Ints here
  case Map.lookup key varCache of
    Just var -> return $ Left var
    Nothing  -> do
      put $ St (nextVar+1) (Map.insert key nextVar varCache)
      return $ Right nextVar
