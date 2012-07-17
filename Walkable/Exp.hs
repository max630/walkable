{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module Walkable.Exp where

import Walkable.Class
import Walkable.Template

import Language.Haskell.TH.Syntax (Quasi)
import Language.Haskell.TH

$(makeInstances (ConT ''Exp) ''Exp (mkName "walkExpImpl")
                [''Pat, ''Name, ''Type, ''Pragma, ''FamFlavour, ''Foreign, ''FunDep, ''Pred, ''Kind, ''Con, ''TyVarBndr, ''Lit]
                [''Dec, ''Match, ''Stmt, ''Range, ''Body, ''Guard, ''Clause, ''Cxt]
                [''FieldExp, ''Exp]
                [])

instance (Quasi m) => Walkable m Exp Exp where
  walk f e = f e
