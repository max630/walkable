{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module Walkable.Exp where

import Walkable.Class
import Walkable.Template

import Language.Haskell.TH.Syntax (Quasi, qRunIO)
import Language.Haskell.TH

$(let
    cycle done result paramType [] = return result
    cycle done result paramType (next : rest) =
      do
        qRunIO $ print (done, (next : rest))
        case () of
          _ | next `elem` empties -> do
            v <- makeEmpty next paramType
            cycle (next : done) (result ++ [v]) paramType rest
          _ | next `elem` reals -> do
            (v, newdeps) <- makeSingleInstance next paramType
            let
              new_done = (next : done)
              filtered_newdeps = filter (\s -> notElem s new_done && notElem s rest && notElem s ignores) newdeps
            cycle new_done (result ++ [v]) paramType (rest ++ filtered_newdeps)
          _ -> fail ("Unknown type: " ++ show next ++ show done)
    empties = [''Pat, ''Name, ''Type, ''Pragma, ''FamFlavour, ''Foreign, ''FunDep, ''Pred, ''Kind, ''Con, ''TyVarBndr, ''Lit]
    reals = [''Dec, ''Match, ''Stmt, ''Range, ''Body, ''Guard, ''Clause]
    ignores = [''FieldExp, ''Exp, ''Cxt]
  in
    do
      (expRes, expDeps) <- makeSingleWalk (mkName "walkExpImpl") ''Exp
      qRunIO $ print (filter (`notElem` ignores) (uniq expDeps))
      cycle [''Exp] [expRes] (ConT ''Exp) (filter (`notElem` ignores) (uniq expDeps) ++ [''Pred])
  )

instance (Quasi m) => Walkable m Exp Exp where
  walk f e = f e
