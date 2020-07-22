module Ludo
    ( play
    , Color(..)
    , numToColor
    ) where

import Control.Monad
import Control.Monad.State
import Data.Map as Map
import System.Random
import Control.Exception
import Text.Read (readMaybe)


data Color = Blue | Green | Red | Yellow deriving (Eq, Ord, Show)

data Piece = Out | Active Int

data Option = Play Int | Move Int Int

data GameState = GameState {
                             turn :: Color
                           , numRolls :: Int
                           , pieces :: Map Color [(Int, Piece)]
                           }

type Ludo a = StateT GameState IO a


instance Show Option where
    show (Play n)    = "play piece " ++ show n
    show (Move n 56) = "send piece " ++ show n ++ " home"
    show (Move n i)  = "move piece " ++ show n ++ " to cell " ++ show i 


numToColor :: Int -> Color
numToColor 1 = Blue
numToColor 2 = Green
numToColor 3 = Yellow
numToColor 4 = Red
numToColor _ = error "Integer must be in the range [1, 4]"


colorToNum :: Color -> Int
colorToNum Blue   = 1
colorToNum Green  = 2
colorToNum Yellow = 3
colorToNum Red    = 4


nextTurn :: Ludo ()
nextTurn = do
    state <- get
    let previous = turn state
    let nextPlayer = numToColor $ ((colorToNum previous) `mod` 4) + 1
    let nextRolls = if Prelude.foldr outCheck True ((pieces state) ! nextPlayer) then 3 else 1
    put state{ turn = nextPlayer, numRolls = nextRolls }

    where
        outCheck (n, Out) True = True
        outCheck _ _ = False


setNumRolls :: Int -> Ludo ()
setNumRolls n = do
    state <- get
    put state{ numRolls = n }


getNumRolls :: Ludo Int
getNumRolls = do
    state <- get
    return $ numRolls state


decrementNumRolls :: Ludo ()
decrementNumRolls = do
    state <- get
    let intermediate = numRolls state
    put state{ numRolls = intermediate - 1 }


initGameState :: Color -> GameState
initGameState player = GameState player 3 pieceMap

    where
        pieceMap = Map.fromList [(player, [(1, Out), (2, Out), (3, Out), (4, Out)]) | player <- [Blue, Green, Red, Yellow]]


play :: Color -> IO Color
play player = do 
    (winner, _) <- runStateT step (initGameState player)
    return winner


step :: Ludo Color
step = do
    rolls <- getNumRolls
    when (rolls < 1) nextTurn
    num <- roll
    player <- playing
    lift $ putStrLn $ show player ++ " rolled " ++ show num
    optionList <- options num
    case optionList of
        []         -> do 
            lift $ putStrLn "no options available, skipping roll"
            decrementNumRolls
            step
        optionList -> do
            option <- promptOption optionList
            applyOption option
            hasWon <- won
            if hasWon
                then playing >>= (\winner -> return winner)
                else do
                    if num == 6 then setNumRolls 1 else setNumRolls 0
                    step


promptOption :: [Option] -> Ludo Option
promptOption optionList = do 
    lift $ putStrLn "select an option:"
    printOptions 1 optionList
    index <- promptIndex 1 (length optionList)
    return $ optionList !! (index - 1)


printOptions :: Int -> [Option] -> Ludo ()
printOptions n [] = return ()
printOptions n (option:options) = do
    lift $ putStrLn $ "(" ++ show n ++ ") " ++ show option
    printOptions (n + 1) options


promptIndex :: Int -> Int -> Ludo Int
promptIndex l h = do
    line <- lift getLine
    case (readMaybe :: String -> Maybe Int) line of
        Nothing -> do
            lift $ putStrLn "invalid option, try again"
            promptIndex l h
        Just n ->
            if n >= l && n <= h
                then return n
                else do
                    lift $ putStrLn "invalid option, try again"
                    promptIndex l h


won :: Ludo Bool
won = return False -- TODO!


applyOption :: Option -> Ludo ()
applyOption option = return () -- TODO!


options :: Int -> Ludo [Option]
options roll = do
    pieces <- playingPieces
    return $ Prelude.foldr addOption [] pieces

    where
        addOption (n, Out) options     = if roll == 6 then (Play n):options else options
        addOption (n, Active i) options
            | i < 51 || roll + i == 56 = (Move n (roll + i)):options
            | otherwise                = options


playingPieces :: Ludo [(Int, Piece)]
playingPieces = do
    state <- get
    let player = turn state
    return $ (pieces state) ! player


playing :: Ludo Color
playing = do
    state <- get
    return $ turn state


roll :: Ludo Int
roll = lift $ getStdRandom (randomR (1, 6))


