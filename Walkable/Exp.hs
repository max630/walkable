{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module Walkable.Exp where

import Walkable.Class
import Walkable.Template

import Language.Haskell.TH.Syntax (Quasi)
import Language.Haskell.TH

$(makeInstances (ConT ''Exp) ''Exp (mkName "walkExpImpl")
                (`elem` [''Pat, ''Name, ''Type, ''Pragma, ''FamFlavour, ''Foreign, ''FunDep, ''Pred, ''Kind, ''Con, ''TyVarBndr, ''Lit])
                (`elem` [''Dec, ''Match, ''Stmt, ''Range, ''Body, ''Guard, ''Clause, ''Cxt])
                (`elem` [''FieldExp, ''Exp]))

instance (Quasi m) => Walkable m Exp Exp where
  walk f e = f e
