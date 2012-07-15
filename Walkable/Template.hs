{-# LANGUAGE TemplateHaskell #-}
module Walkable.Template where

import Walkable.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi)
import Data.List (group, sort)

-- TODO:
-- * bring cycle also here. Make nice to use functions
-- * add type synonims
-- * allow transforming a list of toplevel declarations

makeDecWalk walkName tName =
  do
    f <- newName "f"
    td0 <- reify tName
    case td0 of
      TyConI (DataD [] _ [] tcs []) -> do
              tDatas <- mapM (\ (NormalC conName conTypes) ->
                                  do
                                    let
                                      l = length conTypes
                                      addTypes = concat $ map (\ (_, t) -> getTypes t) conTypes
                                    v0s <- sequence $ map (\n -> newName ("v0_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                                    v1s <- sequence $ map (\n -> newName ("v1_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                                    return (conName, v0s, v1s, addTypes))
                            tcs
              let
                clauseFromtData (conName, v0s, v1s, _) =
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
              return (FunD walkName (map clauseFromtData tDatas), uniq $ concat $ map (\ (_, _, _, ts) -> ts) tDatas)
      _ -> fail ("not a simple data declaration: " ++ show td0)
  where
    getTypes (AppT t1 t2) = (getTypes t1) ++ (getTypes t2)
    getTypes (ConT n) | elem (nameBase n) ["Maybe", "[]", "(,)"] = []
    getTypes (ConT n) = [nameBase n]
    getTypes ListT = []
    getTypes (TupleT _) = []

makeSingleInstance tName paramType =
  do
    m <- newName "m"
    (decWalk, dependencies) <- makeDecWalk 'walk tName
    -- instance Quasi m => Walkable m ,tName ,paramType where
    --  ,decWalk
    return (InstanceD
                  [ClassP ''Quasi [VarT m]]
                  (foldl AppT (ConT ''Walkable) [VarT m, ConT tName, paramType])
                  [decWalk]
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

uniq l = map head $ group $ sort l




