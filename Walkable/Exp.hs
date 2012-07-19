{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, RankNTypes, FlexibleContexts, NoMonomorphismRestriction #-}
module Walkable.Exp where

import Walkable.Class
import Walkable.Template

import Language.Haskell.TH

$(let hasPrefix s p = take (length p) s == p
  in makeInstances (ConT ''Exp) ''Exp (mkName "walkExpImpl")
                (`elem` [''Pat, ''Name, ''Type, ''Pragma, ''FamFlavour, ''Foreign, ''FunDep, ''Pred, ''Kind, ''Con, ''TyVarBndr, ''Lit])
                (\n -> case nameModule n of {Just s | s `hasPrefix` "Language.Haskell.TH." -> True; _ -> False})
                (`elem` [''Exp]))

instance (Monad m) => Walkable m Exp Exp where
  walk f e = f e
