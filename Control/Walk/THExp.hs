{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, Rank2Types, FlexibleContexts, FlexibleInstances #-}
module Control.Walk.THExp where

import Control.Walk.TH
import Control.Walk.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax(NameFlavour, OccName)

import Data.Word (Word8)

$(let hasPrefix s p = take (length p) s == p
  in do
    ([walkExpImplLambda], instancesInfo) <-
      makeTraverseInfo False
                      [''Exp]
                      (`elem` [''String, ''Rational, ''Char, ''Integer, ''Int, ''Word8, ''NameFlavour, ''Bool, ''OccName])
                      (\n -> case nameModule n of {Just s | s `hasPrefix` "Language.Haskell.TH." -> True; _ -> False})
                      (`elem` [''Exp, ''String])
    walkExpImplDec <- [d|
            walkExpImpl :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
            walkExpImpl f = ($(return walkExpImplLambda) :: Monad m => (forall t . Walkable m t (Exp -> m Exp) => t -> m t) -> Exp -> m Exp) (walk f)
          |]
    instancesDec <- mapM (\ (tName, tLambda) -> [d|
                        instance Monad m => Walkable m $(conT tName) (Exp -> m Exp) where
                          walk f = ($(return tLambda) :: Monad m => (forall t . Walkable m t (Exp -> m Exp) => t -> m t) -> $(conT tName) -> m $(conT tName)) (walk f)
                      |])
                      instancesInfo
    return (walkExpImplDec ++ concat instancesDec))

instance Monad m => Walkable m Exp (Exp -> m Exp) where
  walk f e = f e
