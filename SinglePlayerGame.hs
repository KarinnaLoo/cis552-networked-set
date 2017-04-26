module SinglePlayerGame where

import Data.Maybe

import Logic
import qualified Parser as P


---------------------------------- THE GAME ---------------------------------------

createGame :: IO ()
createGame = do
    putStrLn "Created a new game."
    board <- drawCards 12 genAll
    let deck = removeList genAll board
    displayBoard board
    mainLoop deck board

mainLoop :: Deck -> Board -> IO ()
mainLoop deck board = do
    input <- getLine
    let ints = P.getParse parseInP input
    let playedSet = getCards board ints
    if not (playableBoard board) && null deck
      then putStrLn "The game has ended.\n"
    else if input == "exit"
      then putStrLn "You quit.\n"
    else if playableSet playedSet board
    --else if playableSet (P.getParse parseCards input) board
      then do
        let stringSet = removePunc (show (fromJust playedSet)) 
        (deck', board') <- updateBoardAndDeck (fromJust $ P.getParse parseCards stringSet)
                                               deck board
        putStrLn "Nice! you got a set."
        displayBoard board'
        mainLoop deck' board' 
      else do
          putStrLn "Not a valid set or set not in board!"
          mainLoop deck board
