{-# LANGUAGE TemplateHaskell #-}
module Examples.TestLCase where {

import Examples.LCase;

handleLCase [d|

f = lcase [\0 -> 0, \n -> n * f (n - 1)];

|] }
