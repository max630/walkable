{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, EmptyDataDecls, NoMonomorphismRestriction #-}
module THLib where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qRunIO, Quasi)
import System.IO.Unsafe(unsafePerformIO)
import Data.Monoid (mappend, mconcat, mempty)
import IO (hPutStrLn, stderr)
import Monad (mapAndUnzipM)

-- e (VarE n) = litE $ StringL (nameBase n)
-- e _ = litE $ IntegerL 0

spy expQ =
  do
    e <- expQ
    [|let v = $expQ in
       v
       `seq` unsafePerformIO (putStrLn ($(litE $ StringL $ pprint e) ++ ": " ++ show $expQ))
       `seq` v|]

data NotSubstituted = NotSubstituted

-- io :: IO a -> a
io :: IO a -> a 
io = undefined

runIO expQ =
  do
    e <- expQ
    (e', bs) <- walk handleIO e
    qRunIO (print bs)
    return e'

-- TODO: also handle (io $ ..)
handleIO (AppE (VarE ioName) ioExp) | nameBase ioName == "io" && nameModule ioName == Just "THLib" =
  do
    (e', bs') <- walk handleIO ioExp
    name <- newName "io_res"
    return $ Just ((VarE name), bs' `mappend` [(name, e')])
handleIO (DoE sts) = -- FIXME: handle only IO do's (how?)
    do
      sts' <- mapM tr sts
      return $ Just (DoE $ concat sts', mempty)
  where
    tr st =
      do
        (st', bs) <- walkSt handleIO st
        return ((map bindB bs) ++ [st'])
    bindB (name, e) = BindS (VarP name) e
    
  
handleIO _ = return Nothing

walk f e =
  do
    qRunIO (hPutStrLn stderr ("Exp: " ++ show e))
    maybeRes <- f e
    case maybeRes of
      Just res -> return res
      Nothing ->
        case e of
          VarE name ->
            do
              qRunIO (hPutStrLn stderr $"name: " ++ show (nameBase name) ++ " . " ++ show (nameModule name))
              return (e, mempty)
          ConE _ -> return (e, mempty)
          LitE _ -> return (e, mempty)
          AppE e1 e2 -> do
                          (e1', bs1) <- walk f e1
                          (e2', bs2) <- walk f e2
                          return (AppE e1' e2', mappend bs1 bs2)
          InfixE e1 op e2 -> do
                              (e1', bs1) <- case e1 of
                                              Nothing -> return (Nothing, mempty)
                                              Just e -> do {(e', bs') <- walk f e; return (Just e', bs')}
                              (e2', bs2) <- case e2 of
                                              Nothing -> return (Nothing, mempty)
                                              Just e -> do {(e', bs') <- walk f e; return (Just e', bs')}
                              (op', bs3) <- walk f op
                              return (InfixE e1' op' e2', mconcat [bs1, bs3, bs2])
          LamE pats e -> do {(e', bs') <- walk f e; return (LamE pats e', bs')}
          TupE es -> do {(es', bss') <- mapAndUnzipM (\e -> walk f e) es; return (TupE es', mconcat bss')}
          LetE ds e -> do {(e', bs') <- walk f e; return (LetE ds e', bs')} -- is ds really than generic?
          CaseE e ms -> do {(e', bs') <- walk f e; return (CaseE e' ms, bs')} -- TODO: expand ms too
          DoE sts -> do
                      (sts', bss') <- mapAndUnzipM (walkSt f) sts
                      return (DoE sts', mconcat bss')
          CompE sts -> do
                        (sts', bss') <- mapAndUnzipM (walkSt f) sts
                        return (CompE sts', mconcat bss')
          CondE e1 e2 e3 -> do
                              [(e1', bs1'), (e2', bs2'), (e3', bs3')] <-
                                mapM (\e -> walk f e) [e1, e2, e3]
                              return (CondE e1' e2' e3', mconcat [bs1', bs2', bs3'])
          ArithSeqE (FromR e) -> do
                                  (e', bs') <- walk f e
                                  return (ArithSeqE (FromR e), bs')
          ArithSeqE (FromThenR e1 e2) ->
                                 do
                                  (e1', bs1') <- walk f e1
                                  (e2', bs2') <- walk f e2
                                  return (ArithSeqE (FromThenR e1' e2'), mappend bs1' bs2')
          ArithSeqE (FromToR e1 e2) ->
                                 do
                                  (e1', bs1') <- walk f e1
                                  (e2', bs2') <- walk f e2
                                  return (ArithSeqE (FromToR e1' e2'), mappend bs1' bs2')
          ArithSeqE (FromThenToR e1 e2 e3) ->
                                 do
                                  (e1', bs1') <- walk f e1
                                  (e2', bs2') <- walk f e2
                                  (e3', bs3') <- walk f e3
                                  return (ArithSeqE (FromThenToR e1' e2' e3'), mconcat [bs1', bs2', bs3'])
          ListE es -> do
                        (es', bss') <- mapAndUnzipM (walk f) es
                        return (ListE es', mconcat bss')
          SigE e t -> do
                        (e', bs') <- walk f e
                        return (SigE e' t, bs')
          RecConE name fs ->
                      do
                        let (names, es) = unzip fs
                        (es', bss') <- mapAndUnzipM (walk f) es
                        return (RecConE name $ zip names es', mconcat bss')
          RecUpdE e fs ->
                      do
                        (e', bs') <- walk f e
                        let (names, es) = unzip fs
                        (es', bss') <- mapAndUnzipM (walk f) es
                        return (RecUpdE e' $ zip names es', bs' `mappend` mconcat bss')

walkSt f (BindS p e) =
          do
            (e', bs') <- walk f e
            return (BindS p e', bs')
walkSt f (NoBindS e) =
          do
            (e', bs') <- walk f e
            return (NoBindS e', bs')
walkSt f (LetS ds) =
          do
            (ds', bss') <- mapAndUnzipM (walkDec f) ds
            return (LetS ds', mconcat bss')

walkDec f dec@(SigD _ _) = return (dec, mempty)
walkDec f dec@(PragmaD _) = return (dec, mempty)
walkDec f (ValD name body ds) = do
                                  (body', bs1') <- walkBody f body
                                  (ds', bss2') <- mapAndUnzipM (walkDec f) ds
                                  return (ValD name body' ds', bs1' `mappend` mconcat bss2')
walkDec f (FunD name cs) = do
                            (cs', bs') <- mapAndUnzipM (\(Clause ps body ds) -> do {
                                                                                  (body', bs1') <- walkBody f body
                                                                                ; (ds', bss2') <- mapAndUnzipM (walkDec f) ds
                                                                                ; return (Clause ps body' ds', bs1' `mappend` mconcat bss2')})
                                                       cs
                            return (FunD name cs', mconcat bs')

walkBody f (NormalB e) =
                do
                  (e', bs') <- walk f e
                  return (NormalB e', bs')
walkBody f (GuardedB ges) =
                do
                  (ges', bss') <- mapAndUnzipM (\(g, e) -> do {
                                                            (g', bs1') <- walkGuard f g
                                                          ; (e', bs2') <- walk f e
                                                          ; return ((g', e'), mappend bs1' bs2')})
                                               ges
                  return (GuardedB ges', mconcat bss')

walkGuard f (NormalG e) =
                do
                  (e', bs') <- walk f e
                  return (NormalG e', bs')
walkGuard f (PatG sts) =
                do
                  (sts', bss') <- mapAndUnzipM (walkSt f) sts
                  return (PatG sts', mconcat bss')
