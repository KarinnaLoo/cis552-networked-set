module TestClient where

import Control.Monad
import Data.Maybe
import Network
import System.IO

import Logic
import qualified Parser as P


------------------------------ Simulator --------------------------------
main :: IO ()
main = do
  handle   <- connectTo "localhost" (PortNumber 4242)
  boardMsg <- hGetLine handle
  let initialBoard = fromJust $ P.getParse parseBoard boardMsg
  validateGameStart initialBoard
  putStrLn "Initial board:"
  putStrLn $ prettyShowBoard initialBoard
  simulateGame handle initialBoard
  hClose handle
  putStrLn "Successfully finished simulation, exiting."

simulateGame :: Handle -> Board -> IO ()
simulateGame handle oldBoard = do
  let set = findValidSet oldBoard
  if isJust set
    then do
      -- Play another turn
      foundSet <- fromJust set
      hPutStrLn handle (init $ tail $ show foundSet)
      boardMsg <- hGetLine handle
      let newBoard = fromJust $ P.getParse parseBoard boardMsg
      -- Print current game state
      putStrLn "\nSet found:"
      putStrLn $ prettyShowSet foundSet
      putStrLn "New board:"
      putStrLn $ prettyShowBoard newBoard
      -- Validate game state
      validateGameState foundSet oldBoard newBoard
      simulateGame handle newBoard
    else do
      putStrLn "Game has ended."
      validateGameEnd oldBoard

--------------------------- Testing Functions ---------------------------

-- Checks that game state is maintained after a set is found and board is updated
validateGameState :: Set -> Board -> Board -> IO ()
validateGameState set oldBoard newBoard = do
  putStr "Set removed from board: "
  if setRemoved set newBoard
    then putStrLn "Pass"
    else putStrLn "Fail"
  putStr "Board transition valid: "
  if validTransition set oldBoard newBoard
    then putStrLn "Pass"
    else putStrLn "Fail"
  putStr "New board playable:     "
  if playableBoard newBoard
    then putStrLn "Pass"
    else putStrLn "Fail"

-- Checks that initial board is playable
validateGameStart :: Board -> IO ()
validateGameStart board = do
  putStr "Initial board is playable: "
  if playableBoard board
    then putStrLn "Pass"
    else putStrLn "Fail"

-- Checks that there are no sets left when the game ends
validateGameEnd :: Board -> IO ()
validateGameEnd board = do
  putStr "No sets left in board: "
  if not (playableBoard board)
    then putStrLn "Pass"
    else putStrLn "Fail"

-- Checks that set was removed from board
setRemoved :: Set -> Board -> Bool
setRemoved s@(c1, c2, c3) (c : cs) = not (c == c1 || c == c2 || c == c3) && setRemoved s cs
setRemoved _              []       = True

-- Calls first helper unless cards were removed without replacement, then will call second helper
validTransition :: Set -> Board -> Board -> Bool
validTransition set oldBoard newBoard = if length oldBoard > 12 || length newBoard < 12
                                                then validTransition'' set oldBoard newBoard
                                                else validTransition' set oldBoard newBoard

-- Cards in old and new board are in the same positions
validTransition' :: Set -> Board -> Board -> Bool
validTransition' s@(c1, c2, c3) (x : xs) (y : ys) =
                                                    not (x /= y && x /= c1 && x /= c2 && x /= c3)
                                                    && validTransition' s xs ys
validTransition' _ _ _                            = True

-- Cards in old board are still in new board (less strict than above since shifted over)
validTransition'' :: Set -> Board -> Board -> Bool
validTransition'' s@(c1, c2, c3) (x : xs) newBoard =
                                        not (x /= c1 && x /= c2 && x /= c3 && x `notElem` newBoard)
                                        && validTransition'' s xs newBoard 
validTransition'' _ [] _ = True

--------------------------- Helper Functions ---------------------------

-- Finds a valid set from board
findValidSet :: Board -> Maybe (IO Set)
findValidSet []    = Nothing
findValidSet cards = findValidSet' (combinations 3 cards) cards

findValidSet' :: [[Card]] -> Board -> Maybe (IO Set)
findValidSet' ([c1, c2, c3] : cs) cards =
    if validSet (c1, c2, c3)
      then Just (return (c1, c2, c3))
      else findValidSet' cs cards 
findValidSet' _ _ = Nothing
