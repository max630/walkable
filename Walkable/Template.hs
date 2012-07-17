{-# LANGUAGE TemplateHaskell #-}
module Walkable.Template where

import Walkable.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi, qRunIO)
import Data.List (group, sort)

import Control.Monad.Writer(tell, runWriterT)
import Control.Monad.Trans(lift)

import qualified Data.Set as S

-- TODO:
-- * add type expression parameters
-- * add type synonims
-- * allow transforming a list of toplevel declarations

makeInstances paramType startType startName empties reals ignores addDeps = do
  (startRes, startDeps) <- runWriterT (makeSingleWalk startName startType)
  qRunIO $ print (filter (`notElem` ignores) (S.toList startDeps))
  cycle [] [startRes] paramType (filter (`notElem` ignores) (S.toList startDeps) ++ addDeps)
  where
    cycle done result paramType [] = return result
    cycle done result paramType (next : rest) =
      do
        qRunIO $ print (done, (next : rest))
        case () of
          _ | next `elem` empties -> do
            v <- makeEmpty next paramType
            cycle (next : done) (result ++ [v]) paramType rest
          _ | next `elem` reals -> do
            (v, newdeps) <- makeSingleInstance next paramType
            let
              new_done = (next : done)
              filtered_newdeps = filter (\s -> notElem s new_done && notElem s rest && notElem s ignores) newdeps
            cycle new_done (result ++ [v]) paramType (rest ++ filtered_newdeps)
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
              return (FunD walkName (map clauseFromtData tDatas))
      _ -> fail ("not a simple data declaration: " ++ show td0)
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
    return (InstanceD
                  [ClassP ''Quasi [VarT m]]
                  (foldl AppT (ConT ''Walkable) [VarT m, ConT tName, paramType])
                  [decWalk]
           , S.toList dependencies)

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

uniq l = map head $ group $ sort l




