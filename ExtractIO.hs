{-# LANGUAGE TemplateHaskell #-}
module ExtractIO where

import Language.Haskell.TH
import Walkable (walk, walkExpImpl)
import Data.Monoid (mempty, mappend, mconcat)

io :: IO a -> a 
io = undefined

handleIO mainEQ =
  do
    mainE <- mainEQ
    ioE <- [|io|]
    appE <- [|($)|]
    let
      handleIOMatch e =
        do
          (e', bs') <- walkExpImpl handleIOf e
          io_res <- newName "io_res"
          return (VarE io_res, bs' `mappend` [(io_res, e')])
      handleIOf (AppE ioV e) | ioV == ioE = handleIOMatch e
      handleIOf (InfixE (Just ioV) appV (Just e)) | ioV == ioE && appV == appE = handleIOMatch e
      handleIOf (DoE sts) = -- FIXME: handle only IO do's (how?)
        do
          sts' <- mapM tr sts
          return (DoE $ concat sts', mempty)
        where
          tr st =
            do
              (st', bs) <- walk handleIOf st
              return ((map bindB bs) ++ [st'])
          bindB (name, e) = BindS (VarP name) e
      handleIOf e = walkExpImpl handleIOf e
    (mainE', _) <- handleIOf mainE
    return mainE'


