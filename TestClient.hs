module TestClient where

import Control.Monad
import Data.Maybe
import Network
import System.IO

import Logic
import qualified Parser as P

main :: IO ()
main = do
  handle <- connectTo "localhost" (PortNumber 4243)
  simulateGame handle
  hClose handle
  putStrLn "Successfully finished simulation, exiting."

simulateGame :: Handle -> IO ()
simulateGame handle = do
  msg <- hGetLine handle
  putStrLn msg
  let board = fromJust $ P.getParse parseBoard msg
  let set = findValidSet board
  when (isJust set)
    (do
       foundSet <- fromJust set
       hPutStrLn handle (init $ tail $ show $ foundSet)
       simulateGame handle)


--- Helper Functions ---
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

-- getCardIndicesSet :: Set -> Board -> (Int, Int, Int)
-- getCardIndicesSet (c1, c2, c3) b = (getCardIndex c1 b, getCardIndex c2 b, getCardIndex c3 b)

-- getCardIndex :: Card -> Board -> Int
-- getCardIndex card board = getCardIndex' card board 1
--     where getCardIndex' card (c : cs) i = if c == card then i else getCardIndex' card cs (i + 1)
--           getCardIndex' card []       i = error "Card not found in board"
