{-# LANGUAGE TemplateHaskell, LambdaCase, MultiParamTypeClasses, OverlappingInstances, Rank2Types, FlexibleInstances, GADTs #-}
module Examples.LCase where

import Language.Haskell.TH
import Control.Monad(liftM)

import Data.Data(Data, gmapM)

data W :: ((* -> *) -> *) where
  WSingle :: (Data t => t -> m t) -> ExtW m
  WExt :: Data tP => (Data t => t -> m t) -> (tP -> m tP) -> ExtW m

class (Data tP, Data tR) => ExtClass tP tR where
  extSel :: Monad m => (Data t => t -> m t) -> (tP -> m tP) -> tR -> m tR

instance (Data tP, Data tR) => ExtClass tP tR where
  extSel f _ x = f x

instance (Data tP) => ExtClass tP tP where
  extSel _ f x = f x

lcase :: [a -> b] -> a -> b
lcase = undefined

handleLCase expQ = expQ >>= descentLCase

descentLCase :: Data d => d -> Q d
descentLCase = gmapM handler
  where
    handler :: Data d => d -> Q d
    handler = (W recurse) `extM` (\case
                AppE lcaseV clauses | lcaseV == (VarE 'lcase) ->
                  do
                    var <- newName "var"
                    clauses' <- case clauses of
                                  ListE cl -> return cl
                                  _ -> fail ("handleLCase: clauses must me list of lambdas: " ++ show (ppr clauses))
                    clauses'' <-
                      mapM (\v -> case v of
                                    LamE [p] e -> liftM (\e' -> Match p (NormalB e') []) $ descentLCase e
                                    _ -> fail ("handleLCase: only single-pattern lambda is allowed in clauses: " ++ show (ppr v)))
                            clauses'
                    return (LamE [VarP var] (CaseE (VarE var) clauses''))
                e -> recurse e
              )
    -- recurse :: Data d => d -> Q d
    recurse v = gmapM descentLCase v
