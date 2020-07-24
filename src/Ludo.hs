module Ludo
    ( play
    , Color(..)
    , numToColor
    ) where

import Control.Monad
import Control.Monad.State
import Data.Map as Map
import Data.List as List
import System.Random
import Control.Exception
import Text.Read (readMaybe)
import Data.Maybe


data Color = Blue | Green | Red | Yellow deriving (Eq, Ord, Show)

data Piece = Out | Active Int

data Option = Play Int | Move Int Int

data GameState = GameState {
                             turn :: Color
                           , numRolls :: Int
                           , pieces :: Map Color [(Int, Piece)]
                           , finished :: [Color]
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


removeFrom :: [(Int, Piece)] -> Int -> [(Int, Piece)]
removeFrom [] _ = []
removeFrom (piece@(m, _):pieces) n =
    if n == m
        then pieces
        else piece:(removeFrom pieces n)


removeByCell :: [(Int, Piece)] -> Int -> [(Int, Piece)]
removeByCell [] _ = []
removeByCell (piece@(_, Out):pieces) i = piece:(removeFrom pieces i)
removeByCell (piece@(_, Active j):pieces) i =
    if i == j
        then removeFrom pieces i
        else piece:(removeFrom pieces i)


convertCell :: Color -> Int -> Color -> Int
convertCell fromColor cell toColor = (cell + cellOffset) `mod` 52 -- TODO: confirm

    where
        fromNum = colorToNum fromColor
        toNum = colorToNum toColor
        distance = toNum - (fromNum `mod` 4)
        cellOffset = 13 * (4 - distance)


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
initGameState player = GameState player 3 pieceMap []

    where
        pieceMap = Map.fromList [(player, [(1, Out), (2, Out), (3, Out), (4, Out)]) | player <- [Blue, Green, Red, Yellow]]


play :: Color -> IO [Color]
play player = do 
    (winner, _) <- runStateT step (initGameState player)
    return winner


step :: Ludo [Color]
step = do
    player <- playing
    skipPlayer <- colorHasFinished player
    if skipPlayer
        then nextTurn >> step
        else do 
             rolls <- getNumRolls
             when (rolls < 1) nextTurn
             player <- playing
             num <- roll
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
                     hasFinished <- colorHasFinished player
                     when hasFinished ((updateWinOrder player) >> (lift (putStrLn (show player ++ " has finished!"))))
                     gameFinished <- gameHasFinished
                     if gameFinished
                         then getWinOrder >>= (\winOrder -> return winOrder)
                         else (if num == 6 then setNumRolls 1 else setNumRolls 0) >> step


colorHasFinished :: Color -> Ludo Bool
colorHasFinished color = do
    state <- get
    return $ length ((pieces state) ! color) == 0


updateWinOrder :: Color -> Ludo ()
updateWinOrder color = do
    state <- get
    let previousFinished = finished state
    put state{ finished = (previousFinished ++ [color]) }


gameHasFinished :: Ludo Bool
gameHasFinished = do
    state <- get
    return $ length (finished state) == 3


getWinOrder :: Ludo [Color]
getWinOrder = do
    state <- get
    foldM appendUnfinished (finished state) [Blue, Green, Yellow, Red]

    where
        appendUnfinished order color = do
            hasFinished <- colorHasFinished color
            return $ if hasFinished
                        then order
                        else order ++ [color]


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


applyOption :: Option -> Ludo ()
applyOption (Play n) = move n 0 

applyOption (Move n i) = move n i


move :: Int -> Int -> Ludo ()
move n i = do
    state <- get
    let player = turn state
    let allPieces = pieces state
    let colorPieces = allPieces ! player
    case i of
        56 -> do
            let updatedPieces = removeFrom colorPieces n
            put state{ pieces = Map.insert player updatedPieces allPieces }
        _ | i `elem` starCells -> do
            let starIndex = fromJust $ List.elemIndex i starCells
            let newCell = (starCells ++ [56]) !! (starIndex + 1)
            let updatedPieces = 
                    if starIndex + 1 == length starCells 
                        then removeFrom colorPieces n 
                        else (n, Active newCell):(removeFrom colorPieces n)
            put state{ pieces = Map.insert player updatedPieces allPieces }
            player `movesTo` i
            active <- player `occupies` i
            when active (player `movesTo` newCell)
          | i `elem` globeCells -> do
            let updatedPieces = (n, Active i):(removeFrom colorPieces n)
            put state{ pieces = Map.insert player updatedPieces allPieces }
            when (Blue /= player) (handleGlobe Blue)
            when (Green /= player) (handleGlobe Green)
            when (Yellow /= player) (handleGlobe Yellow)
            when (Red /= player) (handleGlobe Red)
          | i >= 51 -> do
            let updatedPieces = (n, Active i):(removeFrom colorPieces n)
            put state{ pieces = Map.insert player updatedPieces allPieces }
          | otherwise -> do
            let updatedPieces = (n, Active i):(removeFrom colorPieces n)
            put state{ pieces = Map.insert player updatedPieces allPieces }
            player `movesTo` i

    where
        starCells  = [5, 11, 18, 24, 31, 37, 44, 50]
        globeCells = [8, 13, 21, 26, 34, 39, 47]
        handleGlobe color = do 
            player <- playing
            isOccupying <- color `occupies` (convertCell player i color)
            when (isOccupying) (player `eliminatedAt` i)


occupies :: Color -> Int -> Ludo Bool
occupies color i = do
    state <- get
    let colorPieces = (pieces state) ! color
    return $ Prelude.foldr isOn False colorPieces

    where
        isOn (_, Out) b      = b
        isOn (_, Active j) b = if i == j then True else b


isOutnumberedAt :: Color -> Int -> Ludo Bool
isOutnumberedAt color i = foldM hasTwo False [Blue, Green, Yellow, Red]

    where
        getPieces = do
            state <- get
            return $ pieces state

        incrementIfEqual j n = if i == j then n + 1 else n

        hasTwo b color' =
            if color == color'
                then return b
                else do
                    allPieces <- getPieces
                    let consPosition (_, piece) list = 
                            case piece of
                                Active j -> (convertCell color' j color):list
                                Out      -> list
                    let positions = Prelude.foldr consPosition [] (allPieces ! color')
                    return $ b || (Prelude.foldr incrementIfEqual 0 positions) >= 2


movesTo :: Color -> Int -> Ludo ()
movesTo color i = do
    outnumbered <- color `isOutnumberedAt` i
    if outnumbered
        then color `eliminatedAt` i
        else do
            when (Blue /= color)   (Blue `eliminatedAt` (convertCell color i Blue))
            when (Green /= color)  (Green `eliminatedAt` (convertCell color i Green))
            when (Yellow /= color) (Yellow `eliminatedAt` (convertCell color i Yellow))
            when (Red /= color)    (Red `eliminatedAt` (convertCell color i Red))


eliminatedAt :: Color -> Int -> Ludo ()
eliminatedAt color i = do
    state <- get
    let allPieces = pieces state
    put state{ pieces = Map.insert Blue (removeByCell (allPieces ! Blue) i) allPieces }


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


