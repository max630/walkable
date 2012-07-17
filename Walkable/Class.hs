{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Arrows #-}
module Walkable.Class where

import Control.Monad (liftM)
import Control.Arrow

-- TODO:
-- * generic parameter type (this probably requires another class)
class (Arrow a) => Walkable a u v where
  walk :: a u u -> a v v

-- Walkable uses m to handle context. For example, Writer can keep scope variables.
-- Also input can also be

instance (Arrow a, Walkable a u v1, Walkable a u v2) => Walkable a u (v1, v2) where
  walk f =
    proc (v1, v2) -> do
      v1' <- walk f -< v1
      v2' <- walk f -< v2
      returnA -< (v1', v2')

instance (ArrowChoice a, Walkable a u v) => Walkable a u (Maybe v) where
  walk f =
    proc vM -> case vM of
      Nothing -> returnA -< Nothing
      Just v -> do
        v' <- walk f -< v
        returnA -< Just v'
        
instance (ArrowChoice a, Walkable a u v) => Walkable a u [v] where
  walk f =
    proc vL -> case vL of
      [] -> returnA -< []
      (v : vT) -> do
        v' <- walk f -< v
        vT' <- walk f -< vT
        returnA -< (v' : vT')

{-
instance (Monad m, Walkable m a b) => Walkable m [a] b where
  walk f es = mapM (walk f) es
-}
