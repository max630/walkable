{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module LCase where

import Language.Haskell.TH
import Walkable (walk, walkExpImpl)

lcase :: [a -> b] -> a -> b
lcase = undefined

handleLCaseF lcaseE (AppE lcaseV clauses) | lcaseV == lcaseE =
  do
    var <- newName "var"
    clauses' <- case clauses of
                  ListE cl -> return cl
                  _ -> fail ("handleLCase: clauses must me list of lambdas: " ++ show (ppr clauses))
    clauses'' <-
      mapM (\v -> case v of
                    LamE [p] e -> do
                      (e', _) <- walk (handleLCaseF lcaseE) e
                      return $ Match p (NormalB e') []
                    _ -> fail ("handleLCase: only single-pattern lambda is allowed in clauses: " ++ show (ppr v)))
            clauses'
    return (LamE [VarP var] (CaseE (VarE var) clauses''), ())
handleLCaseF lcaseE exp = walkExpImpl (handleLCaseF lcaseE) exp

handleLCase expQ = do
  lcaseE <- [|lcase|]
  exp <- expQ
  (res, _) <- handleLCaseF lcaseE exp
  return res
