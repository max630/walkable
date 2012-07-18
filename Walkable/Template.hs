{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Walkable.Template where

import Walkable.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi, qRunIO)
import Language.Haskell.TH.Ppr (ppr)

import Control.Monad.Writer(tell, runWriterT)
import Control.Monad.Trans(lift)

import Data.Maybe(maybeToList)

import qualified Data.Set as S

-- TODO:
-- * allow transforming a list of toplevel declarations

makeInstances paramType startType startName empties reals (S.fromList -> ignores) (S.fromList -> addDeps) = do
  -- TODO: error reporting
  -- ,startName = ..., to put into the handler
  (Just startRes, startDeps) <- runWriterT (makeSingleWalk startName startType)
  cycle S.empty [startRes] paramType ((`S.difference` ignores) startDeps `S.union` addDeps)
  where
    cycle done result paramType (S.minView -> Nothing) = return result
    cycle done result paramType todo@(S.minView -> Just (next, rest)) =
      do
        case () of
          _ | next `elem` empties -> do
            inst <- makeEmpty next paramType
            qRunIO $ putStrLn ("Done empty: " ++ show (ppr next))
            cycle (S.insert next done) (result ++ [inst]) paramType rest
          _ | next `elem` reals -> do
            (inst, newdeps) <- makeSingleInstance next paramType
            qRunIO $ putStrLn ("Done recurse: " ++ show (ppr next))
            let
              new_done = S.insert next done
              filtered_newdeps = (`S.difference` new_done) $ (`S.difference` ignores) newdeps
            cycle new_done (result ++ maybeToList inst) paramType (S.union rest filtered_newdeps)
          _ -> fail ("Unknown type: " ++ show next ++ show done)

makeSingleWalk walkName tName =
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
              let
                clauseFromtData (conName, v0s, v1s) =
                         -- \ ,f (,conName ,v0s[0] ...) -> do
                         --     ,v1s[0], <- walk ,f ,v0s[0]
                         --     ...
                         --     return (,conName ,v1s[0] ,v1s[1] ...)
                         Clause [VarP f, ConP conName (map VarP v0s)]
                            (NormalB (DoE (
                                      zipWith (\v0 v1 ->
                                                  BindS (VarP v1)
                                                        (foldl AppE (VarE 'walk) [VarE f, VarE v0]))
                                               v0s v1s
                                      ++ [NoBindS (AppE (VarE 'return)
                                                        (foldl AppE (ConE conName) (map VarE v1s)))]
                                    ))) []
              return $ Just (FunD walkName (map clauseFromtData tDatas))
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
    m <- newName "m"
    (decWalk, dependencies) <- runWriterT $ makeSingleWalk 'walk tName
    -- instance Quasi m => Walkable m ,tName ,paramType where
    --  ,decWalk
    return (fmap  (\d -> InstanceD
                            [ClassP ''Quasi [VarT m]]
                            (foldl AppT (ConT ''Walkable) [VarT m, ConT tName, paramType])
                            [d])
                  decWalk
           , dependencies)

makeEmpty tName paramType =
  do
    m <- newName "m"
    f <- newName "f"
    e <- newName "e"
    -- instance Quasi m => Walkable m ,tName ,paramType where
    --  walk ,f ,e = return ,e
    return $ InstanceD [ClassP ''Quasi [VarT m]]
                       (foldl AppT (ConT ''Walkable) [VarT m, ConT tName, paramType])
                       [FunD 'walk
                             [Clause [VarP f, VarP e]
                                     (NormalB $ AppE (VarE 'return) (VarE e))
                                     []]]
