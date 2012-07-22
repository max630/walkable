{-# LANGUAGE TemplateHaskell, ViewPatterns, RankNTypes #-}
module Walkable.Template (makeTraverseInfo, makeTraverseLambda) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qRunIO)
import Language.Haskell.TH.Ppr (ppr)

import Control.Monad.Writer(WriterT, tell, runWriterT)
import Control.Monad.Trans(lift)

import Data.Maybe(maybeToList)

import qualified Data.Set as S

-- TODO:
-- * think about not making instances for empties at all, rather copying them in method
-- ** this might be bad for polymorph case
-- * (+) make it independent from concrete class (promising?)
-- * allow transforming a list of toplevel declarations (this one in Exp)
-- * doc
-- * ability to handle parametrized types

{-|
  Returns building blocks for building a hierarchy of instances.

  See an example in 'Walkable.Exp.walkExpImpl'

  The main building block is lambda expression @\ recurse arg -> ...@,
  which decomposes @arg@ to fields, applies @recurse@ to each, and combines it back.
  To use it to implement traversing instance or start functions, apply it to
  closure containing the traversing class method.
  The closure must be polymorthic by the @arg@ type, and instance must exist for it.
  Usually explicit type declaration are required to help typechecker.

  The return type is @(startLabmdas, instancesInfo)@, where each element in @startLabmdas@
  returns lambda for corresponding element of start type names given to the function.
  @instancesInfo@ is a list of @(typeName, typeLambda)@ pairs, which should be used
  to define an instance for the named type with the corresponding instance.

  First @ignore@ is checked, then @empty@, and the last is @real@. If all fails, error is reported.

  Ignored instanced are just skipped from generation, they still might be (and usually are) required.
  Usually this means that you define instances manually. Start types are ignored also.

  Empty instances just copy data argument. Use it for types you are not going to modify,
  because it is not needed of not possible to implement.
 -}
makeTraverseInfo :: [Name] -- ^ list of start type names to start with
                  -> (Name -> Bool) -- ^ if evaluates to true, instance just copies its argument
                  -> (Name -> Bool) -- ^ if evaluates to true, instance recuces to fields
                  -> (Name -> Bool) -- ^ if evaluates to true, type is ignored at all
                  -> Q ([Exp], [(Name, Exp)])
makeTraverseInfo startTypeNames empty real ignore = do
  (startLambdas, startDeps) <- runWriterT $ mapM (\n -> makeTraverseLambda n >>= unMaybe n) startTypeNames
  instances <- cycle S.empty [] (S.filter (not . ignore) startDeps)
  return (startLambdas, instances)
  where
    unMaybe _ (Just v) = return v
    unMaybe n Nothing = fail ("No traverse lambda for: " ++ show n)
    cycle done result (S.minView -> Nothing) = return result
    cycle done result todo@(S.minView -> Just (next, rest)) = do
      (lambdaMb, newdeps) <- runWriterT $ makeLambda next empty real
      qRunIO $ putStrLn ("Done lambda: " ++ show (ppr next) ++ maybe " (no instance)" (const "") lambdaMb)
      let
        new_done = S.insert next done
        filtered_newdeps = (`S.difference` new_done) $ S.filter (not . ignore) newdeps
      cycle new_done (result ++ zip [next] (maybeToList lambdaMb)) (S.union rest filtered_newdeps)

makeLambda :: Name -> (Name -> Bool) -> (Name -> Bool) -> WriterT (S.Set Name) Q (Maybe Exp)
makeLambda tName empty real
  | empty tName = lift $ fmap Just $ makeTrivialLambda
  | real tName = makeTraverseLambda tName
  | True = fail ("Unknown type: " ++ show tName)

-- TODO: document
makeTraverseLambda :: Name -> WriterT (S.Set Name) Q (Maybe Exp)
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

makeTrivialLambda = [|\_ d -> return d|]
