{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Walkable.Class where

import Control.Monad (liftM)
import Language.Haskell.TH.Syntax (Quasi)

-- TODO:
-- * generic parameter type (this probably requires another class)
-- * input state?
class (Quasi m) => Walkable m a b where
  walk :: (b -> m b) -> a -> m a

instance (Quasi m, Walkable m a1 b, Walkable m a2 b) => Walkable m (a1, a2) b where
  walk f (e1, e2) =
    do
      e1' <- walk f e1
      e2' <- walk f e2
      return (e1', e2')

instance (Quasi m, Walkable m a b) => Walkable m [a] b where
  walk f es = mapM (walk f) es

instance (Quasi m, Walkable m a b) => Walkable m (Maybe a) b where
  walk f Nothing = return Nothing
  walk f (Just v) = liftM Just (walk f v)
