module Main where

import MockGame
import Network
import System.IO
import Control.Concurrent

-- Shouldn't need to use this... Why doesn't return () exit all threads when called from main?
-- Also, hClose handle seems to never terminate
foreign import ccall "exit" exit :: IO ()

main :: IO ()
main = do
    putStrLn "Welcome to Set.\nType 'join' to join an existing game or 'host' to create a new game."
    str <- getLine
    case str of
     "host" -> hostGame >> exit
     "join" -> joinGame >> exit
     "exit" -> return ()
     _      -> putStrLn "Unknown command" >> main


hostGame :: IO ()
hostGame = withSocketsDo $ do
    sock <- listenOn $ PortNumber 4242
    putStrLn "Starting server. Waiting for a client to connect..."
    handleConnections sock
    sClose sock

handleConnections :: Socket -> IO ()
handleConnections sock = withSocketsDo $ do
    (handle, _, _) <- accept sock
    putStrLn "Client has connected!"
    -- Create chan for communication, pass it into the following three threads:
    -- Fork off gamestate thread (which will send the msg to client with initial game state,
    -- takes in boolean saying whether it's a server or not)
    -- Fork off user input thread
    -- Fork off networking thread
    -- That's it for now (maybe check if other client quits?)
    -- chan <- newChan
    -- _ <- forkIO (MockGame.createGame handle chan True)
    -- _ <- forkIO (getUserInput chan)
    -- _ <- forkIO (getNetworkMsg handle chan)
    -- return ()
    setupGameThreads handle True
    -- output <- hGetLine handle
    -- putStrLn output
    -- handleConnections sock

joinGame :: IO ()
joinGame = do
    handle <- connectTo "localhost" (PortNumber 4242)
    setupGameThreads handle False

getUserInput :: Chan (Int, String) -> IO ()
getUserInput chan = do
    str <- getLine
    writeChan chan (0, str)
    getUserInput chan

getNetworkMsg :: Handle -> Chan (Int, String) -> IO ()
getNetworkMsg handle chan = do
    msg <- hGetLine handle
    writeChan chan (1, msg)
    getNetworkMsg handle chan

forkGameThread :: Handle -> Chan (Int, String) -> Bool -> IO (MVar ())
forkGameThread handle chan isServer = do
    mv <- newEmptyMVar
    _ <- forkFinally (MockGame.createGame handle chan isServer) (\_ -> putMVar mv ())
    return mv

setupGameThreads :: Handle -> Bool -> IO ()
setupGameThreads handle isServer = do
    chan <- newChan
    --_ <- forkIO (MockGame.createGame handle chan isServer)
    _ <- forkIO (getUserInput chan)
    _ <- forkIO (getNetworkMsg handle chan)
    mvGame <- forkGameThread handle chan isServer
    takeMVar mvGame -- Blocks until the game state thread returns
