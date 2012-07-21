{-# LANGUAGE TemplateHaskell, ViewPatterns, RankNTypes #-}
module Walkable.Template where

import Walkable.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qRunIO)
import Language.Haskell.TH.Ppr (ppr)

import Control.Monad.Writer(tell, runWriterT)
import Control.Monad.Trans(lift)

import Data.Maybe(maybeToList)

import qualified Data.Traversable as T

import qualified Data.Set as S

-- RM: types
import Control.Monad.Writer(WriterT)

-- TODO:
-- * make empty also not producing, censor empties
-- * think about not making instances for empties at all, rather copying them in method
-- ** this might be bad for polymorph case
-- * make it independent from concrete class (promising?)
-- ** (+) work in progress, refactored makeSingleWalk
-- ** (+) MOVE d UNDEL LAMBDA, to avoid handling it in template!!! (see the plan)
-- ** makeEmpty still to go, then can
-- ** start splitting it to core and client code
-- * allow transforming a list of toplevel declarations

-- TODO independent from class:
-- currently there is:
-- instance Walkable m ,tName ,paramType where
--  walk f (C v1 v2) = ... combination of (walk f vN)
-- what is wanted from template is the "combination" and "tName". All the rest can be submitted by client
-- (monad context is also mandatory for now)
-- also, what parameters are added, there can be portion added to the context
-- the proposed solution is make client provide function
-- \,typeName ,comb -> (instance .. ,typeName .. where
--                       walk f d = ,comb (walk f) d)
-- this way, all class-specific info will be brought completely out of template

-- recurses through types and generates:
-- ,startName :: {- makeInstances::startFuncType -} Monad m (,paramType -> m ,paramType) -> ,startType -> m ,startType
-- ,startName = .. same as makeSingleWalk
--
-- NB: refactoring in process!!! might be obsolete
-- {- makeSingleInstance -}
-- instance Walkable ,tName ,paramType where
--  {- makeSingleWalk -} 
--  walk ,f = ((\,wC ,d -> case ,d of
--                  {- makeSingleWalk::clauseFromtData -}
--                  (,conName ,v0s[0] ...) -> do
--                    ,v1s[0], <- ,wC ,v0s[0]
--                    ...
--                    return (,conName ,v1s[0] ,v1s[1] ...)
--                    ...
--                  ) :: {- makeSingleWalk::cType -} Monad m (forall v . Walkable v paramType => v -> m v) -> ,tName -> m ,tName) (walk ,f)
--
-- {- makeEmpty -}
-- instance Walkable ,tName ,paramType where
--  walk ,f ,e = return ,e
--
makeInstances paramType startType startName empty real ignore = do
  -- TODO: error reporting
  startFuncType <- [t|Monad m => ($(return paramType) -> m $(return paramType)) -> $(conT startType) -> m $(conT startType)|]
  (Just startLambda, startDeps) <- runWriterT (makeTraverseLambda startType)
  startRes <- makeSingleWalk startLambda startName startType paramType
  cycle S.empty [SigD startName startFuncType, startRes] paramType (S.filter (not . ignore) startDeps)
  where
    cycle done result paramType (S.minView -> Nothing) = return result
    cycle done result paramType todo@(S.minView -> Just (next, rest)) = do
      (lambdaMb, newdeps) <- runWriterT $ makeLambda next empty real
      inst <- T.mapM (\l -> makeSingleInstance l next paramType) lambdaMb
      qRunIO $ putStrLn ("Done lambda: " ++ show (ppr next) ++ maybe " (no instance)" (const "") lambdaMb)
      let
        new_done = S.insert next done
        filtered_newdeps = (`S.difference` new_done) $ S.filter (not . ignore) newdeps
      cycle new_done (result ++ maybeToList inst) paramType (S.union rest filtered_newdeps)

makeLambda :: Name -> (Name -> Bool) -> (Name -> Bool) -> WriterT (S.Set Name) Q (Maybe Exp)
makeLambda tName empty real
  | empty tName = lift $ fmap Just $ makeTrivialLambda
  | real tName = makeTraverseLambda tName
  | True = fail ("Unknown type: " ++ show tName)


makeSingleWalk lambda walkName tName paramType =
  do
    f <- newName "f"
    cType <- [t|Monad m => (forall v . Walkable v $(return paramType) => v -> m v) -> $(conT tName) -> m $(conT tName)|]
    walkClosure <- [|walk $(varE f)  |]
    return $ FunD walkName [Clause [VarP f] (NormalB $ AppE (SigE lambda cType) walkClosure) []]

makeTraverseLambda tName = do
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
            return $ Just (LamE [VarP wC, VarP d] $ CaseE (VarE d) (map clauseFromtData tDatas))
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

makeSingleInstance lambda tName paramType =
  do
    decWalk <- makeSingleWalk lambda 'walk tName paramType
    return $ InstanceD [] (foldl AppT (ConT ''Walkable) [ConT tName, paramType]) [decWalk]

makeTrivialLambda = [|\_ d -> return d|]
