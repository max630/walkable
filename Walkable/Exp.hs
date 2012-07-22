{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, Rank2Types, FlexibleContexts, FlexibleInstances #-}
module Walkable.Exp where

import Walkable.Class
import Walkable.Template

import Language.Haskell.TH

$(let hasPrefix s p = take (length p) s == p
  in do
    ([walkExpImplLambda], instancesInfo) <-
      makeTraverseInfo [''Exp]
                      (`elem` [''Pat, ''Name, ''Type, ''Pragma, ''FamFlavour, ''Foreign, ''FunDep, ''Pred, ''Kind, ''Con, ''TyVarBndr, ''Lit])
                      (\n -> case nameModule n of {Just s | s `hasPrefix` "Language.Haskell.TH." -> True; _ -> False})
                      (`elem` [''Exp])
    walkExpImplDec <- [d|
            walkExpImpl :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
            walkExpImpl f = ($(return walkExpImplLambda) :: Monad m => (forall t . Walkable t Exp => t -> m t) -> Exp -> m Exp) (walk f)
          |]
    instancesDec <- mapM (\ (tName, tLambda) -> [d|
                        instance Walkable $(conT tName) Exp where
                          walk f = ($(return tLambda) :: Monad m => (forall t . Walkable t Exp => t -> m t) -> $(conT tName) -> m $(conT tName)) (walk f)
                      |])
                      instancesInfo
    return (walkExpImplDec ++ concat instancesDec))

instance Walkable Exp Exp where
  walk f e = f e
