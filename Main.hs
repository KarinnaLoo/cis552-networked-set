module Main where

import Network
import System.IO
import Control.Concurrent
import Control.Exception (catch, IOException)
import Control.Monad
import Data.Maybe

import MultiplayerGame


main :: IO ()
main = do
    putStrLn $ "~~~~~~~~~~~~~~~\nWelcome to Set.\n~~~~~~~~~~~~~~~\n" ++
               "Type 'join' to join an existing game or 'host' to create a new game."
    str <- getLine
    case str of
     "host" -> hostGame >> main
     "join" -> joinGame >> main
     "exit" -> return ()
     _      -> putStrLn "Unknown command\n" >> main


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
    putStrLn "IP address of host: "
    addr <- getLine
    mHandle <- catch (fmap Just (connectTo addr (PortNumber 4242)))
                     (\e -> do
                              let _ = e :: IOException -- Need to specify type of e to compile
                              putStrLn "Couldn't connect to server.\n"
                              return Nothing)
    when (isJust mHandle)
         (do
            let handle = fromJust mHandle
            setupGameThreads handle False
            hClose handle)

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
