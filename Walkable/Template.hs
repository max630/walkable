{-# LANGUAGE TemplateHaskell #-}
module Walkable.Template where

import Walkable.Class

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi, qRunIO)
import Data.List (group, sort)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import System.IO.Unsafe (unsafePerformIO)

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
                                    stOs <- sequence $ map (\n -> newName ("stO_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                                    return (conName, v0s, v1s, stOs, addTypes))
                            tcs
              let
                clauseFromtData (conName, v0s, v1s, stOs, _) =
                         -- \ ,f (,conName ,v0s[0] ...) -> do
                         --     (,v1s[0], ,stOs[0]) <- walk ,f ,v0s[0]
                         --     ...
                         --     return (,conName ,v1s[0] ,v1s[1] ..., mconcat [,stOs[0], ,stOs[1] ...])
                         Clause [VarP f, ConP conName (map VarP v0s)]
                            (NormalB (DoE (
                                      zipWith3 (\v0 v1 stO ->
                                                  BindS (TupP [VarP v1, VarP stO])
                                                        (foldl AppE (VarE $ mkName "walk") [VarE f, VarE v0]))
                                               v0s v1s stOs
                                      ++ [NoBindS (AppE (VarE $ mkName "return")
                                                        (TupE [foldl AppE (ConE conName) (map VarE v1s), AppE (VarE $ mkName "mconcat") (ListE $ map VarE stOs)]))]
                                    ))) []
              return (FunD walkName (map clauseFromtData tDatas), uniq $ concat $ map (\ (_, _, _, _, ts) -> ts) tDatas)
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
    (decWalk, dependencies) <- makeDecWalk (mkName "walk") tName
    -- instance Quasi m => Walkable m ,tName ,paramType where
    --  ,decWalk
    return (InstanceD
                  [ClassP (mkName "Quasi") [VarT m]]
                  (foldl AppT (ConT (mkName "Walkable")) [VarT m, ConT tName, paramType])
                  [decWalk]
           , dependencies)

makeEmpty tName paramType =
  do
    m <- newName "m"
    f <- newName "f"
    e <- newName "e"
    -- instance Quasi m => Walkable m ,tName ,paramType where
    --  walk ,f ,e = return (,e, mempty)
    return $ InstanceD [ClassP (mkName "Quasi") [VarT m]]
                       (foldl AppT (ConT (mkName "Walkable")) [VarT m, ConT tName, paramType])
                       [FunD (mkName "walk")
                             [Clause [VarP f, VarP e]
                                     (NormalB $ AppE (VarE $ mkName "return") (TupE [VarE e, VarE (mkName "mempty")]))
                                     []]]

uniq l = map head $ group $ sort l




