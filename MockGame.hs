module MockGame where

import Network
import System.IO
import Control.Monad
import Control.Concurrent

generateBoard :: [Int]
generateBoard = [1, 2, 3]

-- validateSet :: [Int] -> Bool
-- validateSet _ = True

-- parseSet :: String -> [Int]
-- parseSet _ = [4, 5, 6]

createGame :: Handle -> Chan (Int, String) -> Bool -> IO ()
createGame handle chan isServer = withSocketsDo $ do
    putStrLn "Created a new game."
    when isServer (hPutStrLn handle (show generateBoard))
    mainLoop handle chan isServer

mainLoop :: Handle -> Chan (Int, String) -> Bool -> IO ()
mainLoop handle chan isServer = withSocketsDo $ do
    (src, input) <- readChan chan
    if input == "exit"
      then return ()
      else do
        if src == 0 -- 0 means stdin, 1 means a network message (fix this later)
        -- CASE: USER INPUT
        then do
          hPutStr handle (if isServer then "Server: " else "Client: ")
          hPutStrLn handle input
          -- Replace above with code that validates whether the user's input was a valid set, and if so sends a network msg
          -- Here is some very basic sample code:
          -- if (validateSet (parseSet input))
          --   then do
          --     hPutStrLn handle (input)
          --     when isServer (hPutStrLn handle (show generateBoard))
          --   else do
          --     putStrLn "Not a valid set!"
        -- CASE: NETWORK MSG
        else do
          putStrLn input
          -- Replace above with code that prints the set that the other player found
          -- If this player is a client, read the next message atomically and replace the game board
          -- Basic sample code, not including regenerating the board:
          -- putStr "Other player found the set: "
          -- putStrLn input
        mainLoop handle chan isServer
