{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
module Diag where

{-

Idea: go through


-}

import Data.Array.MArray (newListArray, readArray, writeArray, getElems)
import Data.Array.IO (IOArray)
import Data.IORef (readIORef, writeIORef, newIORef, IORef)
import List (sortBy)
import Monad (zipWithM)
import System.IO.Unsafe(unsafePerformIO)
import ExtractIO (io, handleIO)

get a i =
  do
    vr <- readArray a i
    (_,v) <- readIORef vr
    return v

writeElem a i vr =
  do
    (_, v) <- readIORef vr
    writeIORef vr (i, v)
    writeArray a i vr

moveUp a iOld iNew =
  do
    vrM <- readArray a iOld
    sequence_ (map (\i -> readArray a i >>= writeElem a (i - 1)) [(iOld + 1) .. iNew])
    writeElem a iNew vrM

moveDown a iOld iNew =
  do
    vrM <- readArray a iOld
    sequence_ (map (\i -> readArray a i >>= writeElem a (i + 1)) (reverse [iNew .. (iOld - 1)]))
    writeElem a iNew vrM

solve l =
  do
    let s = length l
    l'' <- zipWithM (\x i -> newIORef (i,x)) l [1 .. s]
    a <- (newListArray (1, s) l'' :: IO (IOArray Int (IORef (Int, Int))))
    let l' = sortBy (\vr1 vr2 -> unsafePerformIO (do {
                                                      (_,v1) <- readIORef vr1;
                                                      (_,v2) <- readIORef vr2;
                                                      return $ compare v1 v2}))
                    l''
    step l' a 0

-- need ot be stable sorted

step [] _ sum = return sum
step (next : l) a sum =
  $(handleIO [|
    do
      putStrLn ("step, sum:" ++ show sum ++ ", nexts:")
      nexts <- mapM readIORef (next : l)
      print nexts
      putStrLn "a:"
      es <- getElems a
      sequence_ $ map (\e -> do {print (io $ readIORef e)}) es
      (iM, vM) <- readIORef next
      let pM = getP vM
      putStrLn ("pM:" ++ show pM)
      if iM <= getP vM
        then step l a sum
        else
          do
            if getP (io $ get a pM) > getP vM -- cannot be less, can be more
              then
                do
                  moveDown a iM pM
                  step l a (sum + (iM - pM))
              else
                do
                  moveDown a iM (pM + 1)
                  let sum' = sum + (iM - pM - 1)
                  -- find 
                  Just iZ <- findM (\i -> do {return (getP (io $ get a i) > pM)}) (reverse [1 .. (pM - 1)])
                  moveUp a iZ (pM + 1)
                  step l a (sum' + (pM + 1 - iZ))
    |])

getP = id -- TODO: change it with actual

findM p [] = return Nothing
findM p (x:xs) =
  do
    r <- p x
    if r
      then return (Just x)
      else findM p xs
