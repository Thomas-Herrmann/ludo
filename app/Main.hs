{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste.Foreign
import Haste.Prim (toJSStr)
import System.Random
import Ludo


main :: IO ()
main = do
    putStrLn "starting new game.."
    roll <- getStdRandom (randomR (1, 4))
    let player = numToColor roll
    putStrLn $ "player " ++ show player ++ " was chosen randomly"
    winOrder <- play player
    putStrLn $ "game has concluded!"
    printWinOrder winOrder 0


printWinOrder :: [Color] -> Int -> IO ()
printWinOrder [] _ = return ()
printWinOrder (color:winOrder) place = do
    putStrLn $ show color ++ " finished " ++ (placeString !! place)
    printWinOrder winOrder (place + 1)

    
placeString = ["1st", "2nd", "3rd", "4th"]