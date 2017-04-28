module MultiplayerGame where

import Network
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Maybe

import Logic
import qualified Parser as P


---------------------------- Definitions (Networking) -----------------------------

type ServerFlag = Bool
type Message = String
data InputSource =
    Stdin
  | Network
  deriving (Eq)


---------------------------------- THE GAME ---------------------------------------

-- If server, generates the first board and sends it to the client
createGame :: Handle -> Chan (InputSource, Message) -> ServerFlag -> IO ()
createGame handle chan isServer = withSocketsDo $ do
    putStrLn "Created a new game."
    board <- drawCards 12 genAll
    let deck = removeList genAll board
    (board', deck') <- updateBoardAndDeck [] board deck
    when isServer (hPrint handle board' >> displayBoard board')
    mainLoop handle chan isServer deck' board'

-- Checks whether the game ended and then blocks until input received
mainLoop :: Handle -> Chan (InputSource, Message) -> ServerFlag -> Deck -> Board -> IO ()
mainLoop handle chan isServer deck board = withSocketsDo $
    if not (playableBoard board) && null deck
      then do
        putStrLn "No sets left, the game has ended.\n"
        hPutStrLn handle "exit"
      else do
        (src, input) <- readChan chan
        if input == "exit"
          then putStrLn "You or the other player quit.\n"
          else playTurn handle chan isServer deck board src input

-- Executes the game logic
playTurn :: Handle -> Chan (InputSource, Message) -> ServerFlag ->
            Deck -> Board -> InputSource -> Message -> IO ()
playTurn handle chan isServer deck board src input = withSocketsDo $
    if src == Stdin
    then do
      let ints      = P.getParse parseInP input
      let playedSet = getCards board ints
      let stringSet = removePunc playedSet
      if playableSet playedSet board -- Player found a valid set
        then do
          putStrLn "Nice! You got a set."
          hPutStrLn handle stringSet -- Send set to other player
          putStrLn ("Cards remaining in deck: " ++ show (length deck))
          if isServer
            then serverUpdateGameState stringSet
            else updateGameState board
        else do
          putStrLn "Not a valid set or set not in board!"
          updateGameState board
    else if src == Network then
      if isSet input -- Other player found a valid set
        then do
          putStrLn "\nOther player found the set: "
          let set = fromJust $ P.getParse parseCards input
          putStrLn $ prettyShowSet set
          putStrLn ("Cards remaining in deck: " ++ show (length deck))
          if isServer
            then serverUpdateGameState input
            else updateGameState board
      else if isBoard input -- Server sent board to client
        then do
          let board' = fromJust $ P.getParse parseBoard input
          displayBoard board'
          updateGameState board'
      else
        putStrLn "Received invalid message from other player."
    else
      error "Unknown input source."
    where serverUpdateGameState input' = do
              (deck', board') <- updateBoardAndDeck
                                    (setToList $ fromJust $ P.getParse parseCards input')
                                     deck board
              hPrint handle board' -- Sends the new board to client
              displayBoard board'
              mainLoop handle chan isServer deck' board'
          updateGameState = mainLoop handle chan isServer deck
