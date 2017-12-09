module Lib
    ( someFunc
    ) where

import System.FSNotify (ActionPredicate)

pred :: ActionPredicate
pred f = True

someFunc :: IO ()
someFunc = putStrLn "someFunc"
