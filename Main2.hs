module Main where

import MockGame
import Network
import System.IO
import Control.Concurrent

-- AttoParsec (parsing/combinator library)
--foreign import ccall "exit" exit :: IO ()

main :: IO ()
main = do
    putStrLn "Welcome to Set.\nType 'join' to join an existing game or 'host' to create a new game."
    str <- getLine
    case str of
     "host" -> hostGame >> main
     "join" -> joinGame >> main
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
    setupGameThreads handle True
    hClose handle

joinGame :: IO ()
joinGame = do
    handle <- connectTo "localhost" (PortNumber 4242)
    setupGameThreads handle False
    hClose handle

getUserInput :: Chan (Int, String) -> IO ()
getUserInput chan = do
    str <- getLine
    writeChan chan (0, str)
    getUserInput chan

getNetworkMsg :: Handle -> Chan (Int, String) -> IO ()
getNetworkMsg handle chan = do
    isHandleClosed <- hIsEOF handle
    if isHandleClosed
    then
        writeChan chan (1, "exit")
    else do
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
    -- Forks three threads: 1 reads from stdin, 2 reads network msgs, 3 plays the game
    tid1 <- forkIO (getUserInput chan)
    tid2 <- forkIO (getNetworkMsg handle chan)
    mvGame <- forkGameThread handle chan isServer
    takeMVar mvGame -- Blocks until the game state thread returns
    killThread tid1
    killThread tid2
