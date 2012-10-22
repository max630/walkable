{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, Rank2Types, FlexibleContexts, FlexibleInstances #-}
module Control.Walkable.THExp where

import Control.Walkable.TH
import Control.Walkable.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax(NameFlavour, OccName)

import Data.Word (Word8)

data ExpHandler m = ExpHandler { handleExp :: Exp -> m Exp }

$(let hasPrefix s p = take (length p) s == p
  in do
    ([walkExpImplLambda], instancesInfo) <-
      makeTraverseInfo False
                      [''Exp]
                      (`elem` [''String, ''Rational, ''Char, ''Integer, ''Int, ''Word8, ''NameFlavour, ''Bool, ''OccName])
                      (\n -> case nameModule n of {Just s | s `hasPrefix` "Language.Haskell.TH." -> True; _ -> False})
                      (`elem` [''Exp, ''String])
    walkExpImplDec <- [d|
            walkExpImpl :: Monad m => (ExpHandler m) -> Exp -> m Exp
            walkExpImpl f = ($(return walkExpImplLambda) :: Monad m => (forall t . Walkable t ExpHandler => t -> m t) -> Exp -> m Exp) (walk f)
          |]
    instancesDec <- mapM (\ (tName, tLambda) -> [d|
                        instance Walkable $(conT tName) ExpHandler where
                          walk f = ($(return tLambda) :: Monad m => (forall t . Walkable t ExpHandler => t -> m t) -> $(conT tName) -> m $(conT tName)) (walk f)
                      |])
                      instancesInfo
    return (walkExpImplDec ++ concat instancesDec))

instance Walkable Exp ExpHandler where
  walk f e = handleExp f e
