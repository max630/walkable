{-# LANGUAGE TemplateHaskell, ViewPatterns, RankNTypes #-}
module Walkable.Template where

import Walkable.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qRunIO)
import Language.Haskell.TH.Ppr (ppr)

import Control.Monad.Writer(tell, runWriterT)
import Control.Monad.Trans(lift)

import Data.Maybe(maybeToList)

import qualified Data.Set as S

-- TODO:
-- * make empty also not producing, censor empties
-- * think about not making instances for empties at all, rather copying them in method
-- ** this might be bad for polymorph case
-- * make it independent from concrete class (promising?)
-- ** work in progress, refactored makeSingleWalk, makeEmpty still to go, then can start splitting it to core and client code
-- * allow transforming a list of toplevel declarations

-- TODO independent from class:
-- currently there is:
-- instance Monad m => Walkable m ,tName ,paramType where
--  walk f (C v1 v2) = ... combination of (walk f vN)
-- what is wanted from template is the "combination" and "tName". All the rest can be submitted by client
-- (monad context is also mandatory for now)
-- also, what parameters are added, there can be portion added to the context

-- recurses through types and generates:
-- ,startName :: {- makeInstances::startFuncType -} Monad m (,paramType -> m ,paramType) -> ,startType -> m ,startType
-- ,startName = .. same as makeSingleWalk
--
-- {- makeSingleInstance -}
-- instance Walkable ,tName ,paramType where
--  {- makeSingleWalk -} 
--  walk ,f ,d = ((\,wC -> case ,d of
--                  {- makeSingleWalk::clauseFromtData -}
--                  (,conName ,v0s[0] ...) -> do
--                    ,v1s[0], <- ,wC ,v0s[0]
--                    ...
--                    return (,conName ,v1s[0] ,v1s[1] ...)
--                    ...
--                  ) :: {- makeSingleWalk::cType -} Monad m (forall v . Walkable v => v -> m v) -> m ,tName) (walk ,f)
--
-- {- makeEmpty -}
-- instance Walkable ,tName ,paramType where
--  walk ,f ,e = return ,e
--
makeInstances paramType startType startName empty real ignore = do
  -- TODO: error reporting
  startFuncType <- [t|Monad m => ($(return paramType) -> m $(return paramType)) -> $(conT startType) -> m $(conT startType)|]
  (Just startRes, startDeps) <- runWriterT (makeSingleWalk startName startType paramType)
  cycle S.empty [SigD startName startFuncType, startRes] paramType (S.filter (not . ignore) startDeps)
  where
    cycle done result paramType (S.minView -> Nothing) = return result
    cycle done result paramType todo@(S.minView -> Just (next, rest)) =
      do
        case () of
          _ | empty next -> do
            inst <- makeEmpty next paramType
            qRunIO $ putStrLn ("Done empty: " ++ show (ppr next))
            cycle (S.insert next done) (result ++ [inst]) paramType rest
          _ | real next -> do
            (inst, newdeps) <- makeSingleInstance next paramType
            qRunIO $ putStrLn ("Done recurse: " ++ show (ppr next) ++ maybe " (no instance)" (const "") inst)
            let
              new_done = S.insert next done
              filtered_newdeps = (`S.difference` new_done) $ S.filter (not . ignore) newdeps
            cycle new_done (result ++ maybeToList inst) paramType (S.union rest filtered_newdeps)
          _ -> fail ("Unknown type: " ++ show next ++ show done)

makeSingleWalk walkName tName paramType =
  do
    f <- lift $ newName "f"
    td0 <- lift $ reify tName
    case td0 of
      TyConI (DataD [] _ [] tcs []) -> do
              tDatas <- mapM (\ (NormalC conName conTypes) ->
                                  do
                                    mapM (\(_,t) -> tellTypes t) conTypes
                                    let
                                      l = length conTypes
                                    v0s <- mapM (\n -> lift $ newName ("v0_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                                    v1s <- mapM (\n -> lift $ newName ("v1_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                                    return (conName, v0s, v1s))
                            tcs
              wC <- lift $ newName "wC" -- walkClosure name
              d <- lift $ newName "d" -- data
              let
                clauseFromtData (conName, v0s, v1s) =
                  Match (ConP conName (map VarP v0s))
                     (NormalB (DoE (
                               zipWith (\v0 v1 ->
                                           BindS (VarP v1)
                                                 (AppE (VarE wC) (VarE v0)))
                                        v0s v1s
                               ++ [NoBindS (AppE (VarE 'return)
                                                 (foldl AppE (ConE conName) (map VarE v1s)))]
                             ))) []
              cType <- lift [t|Monad m => (forall v . Walkable v $(return paramType) => v -> m v) -> m $(conT tName)|]
              walkClosure <- lift [|walk $(varE f)  |]
              return $ Just (FunD walkName
                              [
                                Clause [VarP f, VarP d]
                                      (NormalB $ AppE (SigE (LamE [VarP wC] $ CaseE (VarE d) (map clauseFromtData tDatas))
                                                            cType)
                                                      walkClosure)
                                      []
                              ])
      TyConI (TySynD _ [] tp) -> do
              tellTypes tp
              return Nothing
      _ -> fail ("not a supported declaration: " ++ show td0)
  where
    tellTypes (AppT t1 t2) = tellTypes t1 >> tellTypes t2
    tellTypes (ConT n) | elem n [''Maybe, ''[], ''(,)] = return ()
    tellTypes (ConT n) = tell $ S.singleton n
    tellTypes ListT = return ()
    tellTypes (TupleT _) = return ()

makeSingleInstance tName paramType =
  do
    (decWalk, dependencies) <- runWriterT $ makeSingleWalk 'walk tName paramType
    return (fmap  (\d -> InstanceD
                            []
                            (foldl AppT (ConT ''Walkable) [ConT tName, paramType])
                            [d])
                  decWalk
           , dependencies)

-- TODO: make the same as for real
makeEmpty tName paramType =
  do
    f <- newName "f"
    e <- newName "e"
    return $ InstanceD []
                       (foldl AppT (ConT ''Walkable) [ConT tName, paramType])
                       [FunD 'walk
                             [Clause [VarP f, VarP e]
                                     (NormalB $ AppE (VarE 'return) (VarE e))
                                     []]]
