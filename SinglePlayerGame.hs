module SinglePlayerGame where

import Control.Monad (when)
import Data.Maybe

import Logic
import qualified Parser as P


---------------------------------- THE GAME ---------------------------------------

g :: IO ()
g = do
    putStrLn "Created a new game."
    board <- drawCards 12 genAll
    let deck = removeList genAll board
    (board', deck') <- updateBoardAndDeck [] board deck
    displayBoard board'
    mainLoop deck' board'

mainLoop :: Deck -> Board -> IO ()
mainLoop deck board = do
    when (not (playableBoard board) && null deck)
         (putStrLn "No sets left, the game has ended.\n" >> return ())
    input <- getLine
    let ints = P.getParse parseInP input
    let playedSet = getCards board ints
    if input == "exit"
      then putStrLn "You quit.\n"
    else if playableSet playedSet board
      then do
        (deck', board') <- updateBoardAndDeck (setToList $ fromJust $ playedSet)
                                               deck board
        putStrLn "Nice! you got a set."
        putStrLn ("Cards remaining in deck: " ++ (show $ length deck))
        displayBoard board'
        mainLoop deck' board' 
      else do
          putStrLn "Not a valid set or set not in board!"
          mainLoop deck board
