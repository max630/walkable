{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Control.Walk.Class where

import Control.Monad (liftM)

-- TODO:
-- * generic parameter type (this probably requires another class)
class Monad m => Walkable m a b where
  walk :: b -> a -> m a

-- Walkable uses m to handle context. For example, Writer can keep scope variables.
-- Also input can also be

instance (Walkable m a1 b, Walkable m a2 b) => Walkable m (a1, a2) b where
  walk f (e1, e2) =
    do
      e1' <- walk f e1
      e2' <- walk f e2
      return (e1', e2')

instance (Walkable m a1 b, Walkable m a2 b, Walkable m a3 b) => Walkable m (a1, a2, a3) b where
  walk f (e1, e2, e3) =
    do
      e1' <- walk f e1
      e2' <- walk f e2
      e3' <- walk f e3
      return (e1', e2', e3')

instance (Walkable m a b) => Walkable m [a] b where
  walk f es = mapM (walk f) es

instance (Walkable m a b) => Walkable m (Maybe a) b where
  walk f Nothing = return Nothing
  walk f (Just v) = liftM Just (walk f v)
