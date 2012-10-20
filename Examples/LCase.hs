{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Examples.LCase where

import Language.Haskell.TH
import Control.Monad(liftM)

import Control.Walk.Class (walk)
import Control.Walk.THExp (walkExpImpl)

lcase :: [a -> b] -> a -> b
lcase = undefined

handleLCaseF (AppE lcaseV clauses) | lcaseV == (VarE 'lcase) =
  do
    var <- newName "var"
    clauses' <- case clauses of
                  ListE cl -> return cl
                  _ -> fail ("handleLCase: clauses must me list of lambdas: " ++ show (ppr clauses))
    clauses'' <-
      mapM (\v -> case v of
                    LamE [p] e -> liftM (\e' -> Match p (NormalB e') []) $ walk handleLCaseF e
                    _ -> fail ("handleLCase: only single-pattern lambda is allowed in clauses: " ++ show (ppr v)))
            clauses'
    return (LamE [VarP var] (CaseE (VarE var) clauses''))
handleLCaseF exp = walkExpImpl handleLCaseF exp

handleLCase expQ = do
  exp <- expQ
  walk handleLCaseF exp
