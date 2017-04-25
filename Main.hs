module Main where

import MultiplayerGame
import Network
import System.IO
import Control.Concurrent


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
    sock <- listenOn $ PortNumber 4247
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
    handle <- connectTo "localhost" (PortNumber 4247)
    setupGameThreads handle False
    hClose handle

getUserInput :: Chan (InputSource, Message) -> IO ()
getUserInput chan = do
    str <- getLine
    writeChan chan (Stdin, str)
    getUserInput chan

getNetworkMsg :: Handle -> Chan (InputSource, Message) -> IO ()
getNetworkMsg handle chan = do
    isHandleClosed <- hIsEOF handle
    if isHandleClosed
    then
        writeChan chan (Network, "exit")
    else do
        msg <- hGetLine handle
        writeChan chan (Network, msg)
        getNetworkMsg handle chan

forkGameThread :: Handle -> Chan (InputSource, Message) -> ServerFlag -> IO (MVar ())
forkGameThread handle chan isServer = do
    mv <- newEmptyMVar
    _  <- forkFinally (MultiplayerGame.createGame handle chan isServer) (\_ -> putMVar mv ())
    return mv

setupGameThreads :: Handle -> ServerFlag -> IO ()
setupGameThreads handle isServer = do
    chan   <- newChan
    -- Forks three threads: 1 reads from stdin, 2 reads network msgs, 3 plays the game
    tid1   <- forkIO (getUserInput chan)
    tid2   <- forkIO (getNetworkMsg handle chan)
    mvGame <- forkGameThread handle chan isServer
    takeMVar mvGame -- Blocks until the game state thread returns
    killThread tid1
    killThread tid2
