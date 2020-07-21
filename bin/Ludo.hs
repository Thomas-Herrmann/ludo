module Ludo
    ( move
    ) where

move :: Int -> IO ()
move num = putStrLn $ "moved (" ++ (show num) ++ ")"


