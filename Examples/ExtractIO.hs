{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Examples.ExtractIO where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax(Quasi(..))
import Data.Monoid(Monoid(mempty))
import Control.Monad.Writer(MonadWriter(tell, listen), censor, WriterT, runWriterT)
import Control.Monad.Trans(MonadTrans(lift))

import Control.Walkable.THExp (walkExpImpl)
import Control.Walkable.Class (walk)

io :: IO a -> a 
io = undefined

liftT1 f v = lift (f v)
liftT2 f v1 v2 = lift (f v1 v2)

instance (Quasi m, Monoid w) => Quasi (WriterT w m) where
  qNewName = liftT1 qNewName
  qReport = liftT2 qReport
  -- For now leave it, because not sure enought what this qRecover does
  qRecover _ _ = fail "qRecover not implemented for ExtractIO wrapper"
  qLookupName = liftT2 qLookupName
  qReify = liftT1 qReify
  qReifyInstances = liftT2 qReifyInstances
  qLocation = lift qLocation
  qRunIO = liftT1 qRunIO
  qAddDependentFile = liftT1 qAddDependentFile

pop a = censor (const mempty) $ listen a

handleIO mainEQ =
  do
    let
      handleIOMatch e =
        do
          e' <- walkExpImpl handleIOf e
          io_res <- lift (newName "io_res")
          tell [(io_res, e')]
          return (VarE io_res)
      handleIOf (AppE ioV e) | ioV == (VarE 'io) = handleIOMatch e
      handleIOf (InfixE (Just ioV) appV (Just e)) | ioV == (VarE 'io) && appV == (VarE '($)) = handleIOMatch e
      handleIOf (DoE sts) = -- FIXME: handle only IO do's (how?)
        do
          sts' <- mapM tr sts
          return (DoE $ concat sts')
        where
          tr st =
            do
              (st', bs) <- pop (walk handleIOf st)
              return ((map bindB bs) ++ [st'])
          bindB (name, e) = BindS (VarP name) e
      handleIOf e = walkExpImpl handleIOf e
    mainE <- mainEQ
    (mainE', _) <- runWriterT ((walk :: (Exp -> WriterT [(Name, Exp)] Q Exp) -> [Dec] -> WriterT [(Name, Exp)] Q [Dec]) handleIOf mainE)
    return mainE'


