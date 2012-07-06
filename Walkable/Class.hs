{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Walkable.Class where

import Control.Monad (mapAndUnzipM)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Language.Haskell.TH.Syntax (Quasi)

-- TODO:
-- * generic parameter type (this probably requires another class)
-- * input state?
class (Quasi m) => Walkable m a b where
  walk :: Monoid stO => (b -> m (b, stO)) -> a -> m (a, stO)



instance (Quasi m, Walkable m a1 b, Walkable m a2 b) => Walkable m (a1, a2) b where
  walk f (e1, e2) =
    do
      (e1', stO1) <- walk f e1
      (e2', stO2) <- walk f e2
      return ((e1', e2'), mappend stO1 stO2)

instance (Quasi m, Walkable m a b) => Walkable m [a] b where
  walk f es =
    do
      (es', stOs) <- mapAndUnzipM (walk f) es
      return (es', mconcat stOs)

instance (Quasi m, Walkable m a b) => Walkable m (Maybe a) b where
  walk f Nothing = return (Nothing, mempty)
  walk f (Just v) =
    do
      (e', stO) <- walk f v
      return (Just e', stO)
