{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste.Foreign
import Haste.Prim (toJSStr)
import System.Random
import Ludo

--main :: IO ()
--main = do
--    export "move" move


main :: IO ()
main = do
    putStrLn "starting new game.."
    roll <- getStdRandom (randomR (1, 4))
    let player = numToColor roll
    putStrLn $ "player " ++ show player ++ " was chosen randomly"
    winner <- play player
    putStrLn $ "player " ++ show winner ++ " has won!"
