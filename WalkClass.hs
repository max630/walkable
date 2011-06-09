{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module WalkClass where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi, qRunIO)
import Monad (liftM)
import Data.Monoid (Monoid, mempty, mappend, mconcat)

-- TODO: input state?
class (Quasi m) => Walkable m a b where
  walk :: Monoid stO => (b -> m (b, stO)) -> a -> m (a, stO)

-- instance Quasi m => Walkable m Dec where { $(return [FunD (mkName "walk") []]) }

do
  m <- newName "m"
  f <- newName "f"
  TyConI (DataD [] _ [] tcs []) <- reify (mkName "Exp")
  tDatas <- mapM (\(NormalC conName conTypes) ->
                        do
                          let l = length conTypes
                          v0s <- sequence $ map (\n -> newName ("v0_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                          v1s <- sequence $ map (\n -> newName ("v1_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                          stOs <- sequence $ map (\n -> newName ("stO_" ++ nameBase conName ++ "_" ++ show n)) [1 .. l]
                          return (conName, v0s, v1s, stOs))
                 tcs
  qRunIO $ print tDatas
  {-
  let
    clauseFromCon (NormalC conName conTypes) = FunD (mkName "walk") [Clause [VarP f, ConT ]]
    map (clauseFromCon ) tcs
  -}
  let
    clauseFromtData (conName, v0s, v1s, stOs) =
             Clause [VarP f, ConP conName (map VarP v0s)]
                (NormalB (DoE (
                          zipWith3 (\v0 v1 stO ->
                                      BindS (TupP [VarP v1, VarP stO])
                                            (foldl AppE (VarE $ mkName "walk") [VarE f, VarE v0]))
                                   v0s v1s stOs
                          ++ [NoBindS (AppE (VarE $ mkName "return")
                                            (TupE [foldl AppE (ConE conName) (map VarE v1s), AppE (VarE $ mkName "mconcat") (ListE $ map VarE stOs)]))]
                        ))) []
    res = InstanceD
                [ClassP (mkName "Quasi") [VarT m]]
                (foldl AppT (ConT (mkName "Walkable")) [VarT m, ConT (mkName "Exp"), ConT (mkName "Exp")])
                [FunD (mkName "walk") (map clauseFromtData tDatas)]
  qRunIO $ print $ ppr res
  return [res]


{-
liftM concat $ mapM
  (\tQ ->
    do
      t <- tQ
      let p = WildP
      let e = TupE []
      res <- [d|instance Quasi m => Walkable m $tQ where { $(return [] :: Q [Dec]) }|]
      qRunIO $ print res
      return res
  )
  [[t|Exp|]] -}
