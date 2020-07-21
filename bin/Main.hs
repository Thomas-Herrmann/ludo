{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste.Foreign
import Haste.Prim (toJSStr)
import Ludo (move)

main :: IO ()
main = do
    export "move" move
