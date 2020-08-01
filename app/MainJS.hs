{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste.Foreign
import Haste.Prim (toJSStr)
import Haste.DOM
import System.Random
import LudoJS


main :: IO ()
main = do
    export "numPiecesAt" numPiecesAt
    roll <- getStdRandom (randomR (1, 4))
    let player = numToColor roll
    withElem "canvas" $ play player
