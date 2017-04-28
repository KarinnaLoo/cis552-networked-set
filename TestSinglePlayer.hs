module SinglePlayerGame where

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
mainLoop deck board =
    if not (playableBoard board) && null deck
      then putStrLn "No sets left, the game has ended.\n"
    else do
      let playedSet = Just (findValidSet board)
      if playableSet playedSet board
      then do
        (deck', board') <- updateBoardAndDeck
                              (setToList $ fromJust playedSet)
                               deck board
        putStrLn "Nice! you got a set."
        putStrLn ("Cards remaining in deck: " ++ show (length deck))
        displayBoard board'
        mainLoop deck' board' 
      else do
          putStrLn "Not a valid set or set not in board!"
          mainLoop deck board


--------------------------- Helper Functions ---------------------------

-- Finds a valid set from board
findValidSet :: Board -> Set
findValidSet []    = error "findValidSet empty board"
findValidSet cards = findValidSet' (combinations 3 cards) cards

findValidSet' :: [[Card]] -> Board -> Set
findValidSet' ([c1, c2, c3] : cs) cards =
    if validSet (c1, c2, c3)
      then (c1, c2, c3)
      else findValidSet' cs cards 
findValidSet' _ _ = error "findValidSet' no sets on board valid"
