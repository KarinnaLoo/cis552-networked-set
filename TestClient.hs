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
  handle   <- connectTo "localhost" (PortNumber 4243)
  boardMsg <- hGetLine handle
  let initialBoard = fromJust $ P.getParse parseBoard boardMsg
  -- TODO: Validate initial board
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
      hPutStrLn handle (init $ tail $ show $ foundSet)
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
    else
      putStrLn "No sets left in board."
      -- TODO: Validate no sets left

-- TODO: Implement
validateGameState :: Set -> Board -> Board -> IO ()
validateGameState set oldBoard newBoard = putStrLn "Validating..."


--------------------------- Helper Functions ---------------------------
-- Finds a valid set from board
findValidSet :: Board -> Maybe (IO Set)
findValidSet []    = Nothing
findValidSet cards = findValidSet' (combinations 3 cards) cards

-- Helper for above
findValidSet' :: [[Card]] -> Board -> Maybe (IO Set)
findValidSet' ([c1, c2, c3] : cs) cards =
    if validSet (c1, c2, c3)
      then Just (return (c1, c2, c3))
      else findValidSet' cs cards 
findValidSet' _ _ = Nothing
