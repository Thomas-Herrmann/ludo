{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module LudoJS
    ( play
    , Player(..)
    , numToColor
    , convertCell
    ) where

import Control.Monad
import Control.Monad.State as State
import Control.Monad.Loops
import Haste.Events
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Foreign as Foreign
import Haste.Prim
import Data.Map as Map
import Data.List as List
import System.Random
import Control.Exception
import Text.Read (readMaybe)
import Data.Maybe


data Player = Blue | Green | Red | Yellow deriving (Eq, Ord, Show)

data Piece = Out | Active Int

data Option = Play Int | Move Int Int deriving (Eq)

data Stage = Roll | SelectPiece Int | SelectField Int Int | GameFinished deriving Show

data GameState = GameState {
                             stage :: Stage
                           , turn :: Player
                           , numRolls :: Int
                           , pieces :: Map Player [(Int, Piece)]
                           , finished :: [Player]
                           }

type Ludo a = StateT GameState IO a

instance Show Option where
    show (Play n)    = "play piece " ++ show n
    show (Move n 56) = "send piece " ++ show n ++ " home"
    show (Move n i)  = "move piece " ++ show n ++ " to cell " ++ show i 

instance ToAny GameState where
    toAny state = toObject [("stage", toAny $ stage state),
                            ("turn", toAny $ show (turn state)),
                            ("numRolls", toAny $ numRolls state),
                            ("pieces", toAny $ pieces state),
                            ("finished", toAny $ finished state)]

instance ToAny (Map Player [(Int, Piece)]) where
    toAny pieces = toObject [(toJSStr (show player), toObject [(toJSStr (show n), toAny piece) | (n, piece) <- pairList ]) | (player, pairList) <- Map.toList pieces]

instance ToAny Piece where
    toAny Out        = toObject [("piece", toAny (toJSStr "Out"))]
    toAny (Active i) = toObject [("piece", toAny (toJSStr "Active")), ("field", toAny i)]

instance ToAny Stage where
    toAny Roll = toObject [("stage", toAny (toJSStr "Roll"))]
    toAny (SelectPiece n) = toObject [("stage", toAny (toJSStr "SelectPiece")), ("pieceIndex", toAny n)]
    toAny (SelectField n i) = toObject [("stage", toAny (toJSStr "SelectField")), ("pieceIndex", toAny n), ("fieldIndex", toAny i)]
    toAny GameFinished = toObject [("stage", toAny (toJSStr "GameFinished"))]

instance ToAny Option where
    toAny (Play n)   = toObject [("option", toAny (toJSStr "Play")), ("piece", toAny n)]
    toAny (Move n i) = toObject [("option", toAny (toJSStr "Move")), ("piece", toAny n), ("field", toAny i)]

instance ToAny Player where
    toAny player = toAny (show player)

instance FromAny Player where
    fromAny any = do
        string <- (fromAny :: JSAny -> IO String) any
        return $ case string of
            "Blue"   -> Blue
            "Green"  -> Green
            "Yellow" -> Yellow
            "Red"    -> Red


instance FromAny Stage where
    fromAny any = do
    stateString <- (Foreign.get :: JSAny -> JSString -> IO JSString) any "stage"
    case stateString of
        "Roll" -> return Roll
        "SelectPiece"  -> Foreign.get any "pieceIndex" >>= (\n -> return $ SelectPiece n)
        "SelectField"  -> Foreign.get any "pieceIndex" >>= (\n -> Foreign.get any "fieldIndex" >>= (\i -> return $ SelectField n i))
        "GameFinished" -> return GameFinished   


instance FromAny (Map Player [(Int, Piece)]) where
    fromAny any =
        let fromAnyPlayerPieces player = do
            piecesAny <- Foreign.get any $ toJSStr (show player)
            let listBuilder l n = do
                hasN <- Foreign.has piecesAny $ toJSStr (show n)
                if hasN
                    then Foreign.get piecesAny (toJSStr (show n)) >>= (\pairAny -> fromAnyPiecePair pairAny n >>= (\pair -> return $ l ++ [pair]))
                    else return l
            pieces <- foldM listBuilder [] [1, 2, 3, 4]
            return (player, pieces)     
        in Prelude.mapM fromAnyPlayerPieces [Blue, Green, Yellow, Red] >>= (\kVList -> return $ Map.fromList kVList)
        where
            fromAnyPiecePair any n = do
                pieceString <- Foreign.get any "piece"
                case () of
                   _ | pieceString == toJSStr "Out"    -> return (n, Out)
                     | pieceString == toJSStr "Active" -> Foreign.get any "field" >>= (\i -> return (n, Active i))


instance FromAny GameState where
    fromAny any = do
        stage    <- Foreign.get any "stage"
        turn     <- Foreign.get any "turn"
        numRolls <- Foreign.get any "numRolls"
        pieces   <- Foreign.get any "pieces"
        finished <- Foreign.get any "finished"
        return $ GameState stage turn numRolls pieces finished


numToColor :: Int -> Player
numToColor 1 = Blue
numToColor 2 = Green
numToColor 3 = Yellow
numToColor 4 = Red
numToColor _ = error "Integer must be in the range [1, 4]"


colorToNum :: Player -> Int
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


convertCell :: Player -> Int -> Player -> Int
convertCell fromColor cell toColor = (cell + cellOffset) `mod` 52 -- TODO: confirm

    where
        fromNum = colorToNum fromColor
        toNum = colorToNum toColor
        distance = toNum - (fromNum `mod` 4)
        cellOffset = 13 * (4 - distance)


nextTurn :: Ludo ()
nextTurn = do
    state <- State.get
    let previous = turn state
    let nextPlayer = numToColor $ ((colorToNum previous) `mod` 4) + 1
    let nextRolls = if Prelude.foldr outCheck True ((pieces state) ! nextPlayer) then 3 else 1
    put state{ turn = nextPlayer, numRolls = nextRolls }

    where
        outCheck (n, Out) True = True
        outCheck _ _ = False


setNumRolls :: Int -> Ludo ()
setNumRolls n = do
    state <- State.get
    put state{ numRolls = n }


getNumRolls :: Ludo Int
getNumRolls = do
    state <- State.get
    return $ numRolls state


decrementNumRolls :: Ludo ()
decrementNumRolls = do
    state <- State.get
    let intermediate = numRolls state
    put state{ numRolls = intermediate - 1 }


initGameState :: Player -> GameState
initGameState player = GameState Roll player 3 pieceMap []
    where
        pieceMap = Map.fromList [(player, [(1, Out), (2, Out), (3, Out), (4, Out)]) | player <- [Blue, Green, Red, Yellow]]


play :: Player -> Elem -> IO ()
play player canvasEl = (onEvent canvasEl Click clickHandler) >> return ()
    where
        clickHandler (MouseData coords (Just MouseLeft) _) = do
            state <- getGameState
            maybe <- maybeField coords (turn state)
            case maybe of
                Just i  -> do
                    state  <- getGameState
                    state' <- execStateT (step i) state
                    putGameState state'
                Nothing -> return ()

        clickHandler _ = return ()


step :: Int -> Ludo ()
step i = do
    stage <- getStage
    case stage of
        Roll -> when (isDiceField i) roll
        SelectPiece num -> when (isOutField i) $ selectPiece (fieldToPieceIndex i) num
        SelectField num n -> 
            if isActiveField i
                then selectField n i num
                else setStage $ SelectPiece num
        GameFinished -> do
            starterIndex <- lift $ getStdRandom (randomR (1, 4))
            put $ initGameState (numToColor starterIndex)
    drawBoard
    

getGameState :: IO GameState
getGameState = ffi "(() => gameState)"


putGameState :: GameState -> IO ()
putGameState = ffi "((gs) => gameState=gs)"


maybeField :: (Int, Int) -> Player -> IO (Maybe Int)
maybeField (x, y) player = findField x y player
    where
        findField = ffi "((x, y, player) => posToField(x, y, player))" :: Int -> Int -> Player -> IO (Maybe Int)


drawBoard :: Ludo ()
drawBoard = do
    state <- State.get
    let currentStage = stage state
    let roll = getRoll currentStage
    optionList <- if roll /= -1 then options roll else return []
    lift (draw state optionList roll)
    where
        draw = ffi "((gs, opts, rolls) => drawBoard(gs, opts, rolls))" :: GameState -> [Option] -> Int -> IO ()

        getRoll currentStage =
            case currentStage of
                Roll -> -1 -- javascript friendly 'Nothing'
                SelectPiece num   -> num
                SelectField num _ -> num


selectPiece :: Int -> Int -> Ludo ()
selectPiece n num = do
    pieces <- playingPieces
    when (hasPiece pieces) $ setStage (SelectField num n)
    where
        hasPiece [] = False
        hasPiece ((m, _):pieces) = if m == n then True else hasPiece pieces 


selectField :: Int -> Int -> Int -> Ludo ()
selectField n i num = do
    pieces <- playingPieces
    optionList <- options num
    let option = requestToOption pieces
    when (option `elem` optionList) $ do
        applyOption option
        player <- playing
        hasFinished <- colorHasFinished player
        when hasFinished $ updateWinOrder player
        gameFinished <- gameHasFinished
        if gameFinished
            then do
                state <- State.get
                winOrder <- getWinOrder
                put state{ finished = winOrder }
                setStage GameFinished
            else setStage Roll
        setStage $ if gameFinished then GameFinished else Roll
    where
        requestToOption [] = error "piece does not exist"
        requestToOption ((m, piece):pieces) =
            if m == n
                then case piece of
                        Out      -> Play m
                        Active i -> Move n i
                else requestToOption pieces


roll :: Ludo ()
roll = do
    whileM (skipsPlayer >>= (\skips -> getNumRolls >>= (\rolls -> return $ skips || rolls < 1))) nextTurn
    player <- playing
    num <- lift $ getStdRandom (randomR (1, 6))
    optionList <- options num
    case optionList of
        [] -> decrementNumRolls
        _  -> (setStage $ SelectPiece num) >> (setNumRolls $ if num == 6 then 1 else 0)


skipsPlayer :: Ludo Bool
skipsPlayer = do
    player <- playing
    colorHasFinished player


isDiceField :: Int -> Bool
isDiceField i = i == -5


isOutField :: Int -> Bool
isOutField i = i <= -1 && i >= -4


isActiveField :: Int -> Bool
isActiveField i = i <= 56 && i >= 0


getWinOrder :: Ludo [Player]
getWinOrder = do
    state <- State.get
    foldM appendUnfinished (finished state) [Blue, Green, Yellow, Red]

    where
        appendUnfinished order color = do
            hasFinished <- colorHasFinished color
            return $ if hasFinished
                        then order
                        else order ++ [color]


fieldToPieceIndex :: Int -> Int
fieldToPieceIndex i
    | i <= -1 && i >= -4 = i + 5
    | otherwise          = error "invalid field, must be in range [-1, -4]"


setStage :: Stage -> Ludo ()
setStage newStage = do
    state <- State.get
    put state{ stage = newStage }


getStage :: Ludo Stage
getStage = do
    state <- State.get
    return $ stage state


colorHasFinished :: Player -> Ludo Bool
colorHasFinished color = do
    state <- State.get
    return $ length ((pieces state) ! color) == 0


updateWinOrder :: Player -> Ludo ()
updateWinOrder color = do
    state <- State.get
    let previousFinished = finished state
    put state{ finished = (previousFinished ++ [color]) }


gameHasFinished :: Ludo Bool
gameHasFinished = do
    state <- State.get
    return $ length (finished state) == 3


applyOption :: Option -> Ludo ()
applyOption (Play n) = move n 0 

applyOption (Move n i) = move n i


move :: Int -> Int -> Ludo ()
move n i = do
    state <- State.get
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


occupies :: Player -> Int -> Ludo Bool
occupies color i = do
    state <- State.get
    let colorPieces = (pieces state) ! color
    return $ Prelude.foldr isOn False colorPieces

    where
        isOn (_, Out) b      = b
        isOn (_, Active j) b = if i == j then True else b


isOutnumberedAt :: Player -> Int -> Ludo Bool
isOutnumberedAt color i = foldM hasTwo False [Blue, Green, Yellow, Red]

    where
        getPieces = do
            state <- State.get
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


movesTo :: Player -> Int -> Ludo ()
movesTo color i = do
    outnumbered <- color `isOutnumberedAt` i
    if outnumbered
        then color `eliminatedAt` i
        else do
            when (Blue /= color)   (Blue `eliminatedAt` (convertCell color i Blue))
            when (Green /= color)  (Green `eliminatedAt` (convertCell color i Green))
            when (Yellow /= color) (Yellow `eliminatedAt` (convertCell color i Yellow))
            when (Red /= color)    (Red `eliminatedAt` (convertCell color i Red))


eliminatedAt :: Player -> Int -> Ludo ()
eliminatedAt color i = do
    state <- State.get
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
    state <- State.get
    let player = turn state
    return $ (pieces state) ! player


playing :: Ludo Player
playing = do
    state <- State.get
    return $ turn state


