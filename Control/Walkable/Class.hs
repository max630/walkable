{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, KindSignatures #-}
module Control.Walkable.Class where

import Control.Monad (liftM)

class Walkable a (b :: (* -> *) -> *) where
  walk :: Monad m => b m -> a -> m a

-- Walkable uses m to handle context. For example, Writer can keep scope variables.
-- Also input can also be

instance (Walkable a1 b, Walkable a2 b) => Walkable (a1, a2) b where
  walk f (e1, e2) =
    do
      e1' <- walk f e1
      e2' <- walk f e2
      return (e1', e2')

instance (Walkable a1 b, Walkable a2 b, Walkable a3 b) => Walkable (a1, a2, a3) b where
  walk f (e1, e2, e3) =
    do
      e1' <- walk f e1
      e2' <- walk f e2
      e3' <- walk f e3
      return (e1', e2', e3')

instance (Walkable a b) => Walkable [a] b where
  walk f es = mapM (walk f) es

instance (Walkable a b) => Walkable (Maybe a) b where
  walk f Nothing = return Nothing
  walk f (Just v) = liftM Just (walk f v)
