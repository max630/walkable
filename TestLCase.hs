{-# LANGUAGE TemplateHaskell #-}
module TestLCase where

import LCase

$(handleLCase [d|


 f = lcase [\0 -> 0, \n -> n * f (n - 1)]



 |])
