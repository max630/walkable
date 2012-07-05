{-# LANGUAGE TemplateHaskell #-}
module TestLCase where

import LCase

f = $(handleLCase [|lcase [\0 -> 0, \n -> n * f (n - 1)]|])
