{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module WalkClass where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi, qRunIO)
import Control.Monad (mapAndUnzipM)
import Data.List (group, sort)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import System.IO.Unsafe (unsafePerformIO)

-- TODO: input state?
class (Quasi m) => Walkable m a b where
  walk :: Monoid stO => (b -> m (b, stO)) -> a -> m (a, stO)

-- instance Quasi m => Walkable m Dec where { $(return [FunD (mkName "walk") []]) }

$(let
    makeDecWalk walkName tName =
      do
        f <- newName "f"
        td0 <- reify tName
        case td0 of
          TyConI (DataD [] _ [] tcs []) -> return ()
          _ -> fail (show td0)
        let TyConI (DataD [] _ [] tcs []) = td0
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
    getTypes (AppT t1 t2) = (getTypes t1) ++ (getTypes t2)
    getTypes (ConT n) | elem (nameBase n) ["Maybe", "[]", "(,)"] = []
    getTypes (ConT n) = [nameBase n]
    getTypes ListT = []
    getTypes (TupleT _) = []
    makeInstance tName =
      do
        m <- newName "m"
        (decWalk, dependencies) <- makeDecWalk (mkName "walk") tName
        return (InstanceD
                      [ClassP (mkName "Quasi") [VarT m]]
                      (foldl AppT (ConT (mkName "Walkable")) [VarT m, ConT tName, ConT (mkName "Exp")])
                      [decWalk]
               , dependencies)
    makeEmpty tName =
      do
        m <- newName "m"
        f <- newName "f"
        e <- newName "e"
        return $ InstanceD [ClassP (mkName "Quasi") [VarT m]]
                           (foldl AppT (ConT (mkName "Walkable")) [VarT m, ConT tName, ConT (mkName "Exp")])
                           [FunD (mkName "walk")
                                 [Clause [VarP f, VarP e]
                                         (NormalB $ AppE (VarE $ mkName "return") (TupE [VarE e, VarE (mkName "mempty")]))
                                         []]]
    cycle done result [] = return result
    cycle done result (next : rest) =
      do
        qRunIO $ print (done, (next : rest))
        case () of
          _ | next `elem` empties -> do
            v <- makeEmpty (mkName next)
            cycle (next : done) (result ++ [v]) rest
          _ | next `elem` reals -> do
            (v, newdeps) <- makeInstance (mkName next)
            let
              new_done = (next : done)
              filtered_newdeps = filter (\s -> notElem s new_done && notElem s rest && notElem s ignores) newdeps
            cycle new_done (result ++ [v]) (rest ++ filtered_newdeps)
          _ -> fail ("Unknown type: " ++ next ++ show done)
    empties = ["Pat", "Name", "Type", "Pragma", "FamFlavour", "Foreign", "FunDep", "Pred", "Kind", "Con", "TyVarBndr", "Lit"]
    reals = ["Dec", "Match", "Stmt", "Range", "Body", "Guard", "Clause"]
    ignores = ["FieldExp", "Exp", "Cxt"]
    uniq l = map head $ group $ sort l
  in
    do
      (expRes, expDeps) <- makeDecWalk (mkName "walkExpImpl") ''Exp
      qRunIO $ print (filter (`notElem` ignores) (uniq expDeps))
      cycle ["Exp"] [expRes] (filter (`notElem` ignores) (uniq expDeps) ++ ["Pred"])
  )

instance (Quasi m) => Walkable m Exp Exp where
  walk f e = f e


instance (Quasi m, Walkable m a1 b, Walkable m a2 b) => Walkable m (a1, a2) b where
  walk f (e1, e2) =
    do
      (e1', stO1) <- walk f e1
      (e2', stO2) <- walk f e2
      return ((e1', e2'), mappend stO1 stO2)

instance (Quasi m, Walkable m a b) => Walkable m [a] b where
  walk f es =
    do
      (es', stOs) <- mapAndUnzipM (walk f) es
      return (es', mconcat stOs)

instance (Quasi m, Walkable m a b) => Walkable m (Maybe a) b where
  walk f Nothing = return (Nothing, mempty)
  walk f (Just v) =
    do
      (e', stO) <- walk f v
      return (Just e', stO)
