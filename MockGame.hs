module MockGame where

import Network
import System.IO
import Control.Monad
import Control.Concurrent

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative (Alternative(..))
import Data.List (tails, delete)
import Data.List.Split
import System.Random
import Control.Monad (replicateM)

import qualified Parser as P
import qualified ParserCombinators as P

data Board = 
    Board [Card]
  deriving (Eq, Show)

data Card =
    Card Shape Filling Number Color
  deriving (Eq, Show)

data Shape =
    Triangle
  | Squiggle
  | Oval
  deriving (Eq, Show, Enum)

data Filling =
    Shaded
  | Solid
  | Outline
  deriving (Eq, Show, Enum)

data Number = 
    One 
  | Two
  | Three
  deriving (Eq, Show, Enum)

data Color =
    Green
  | Red
  | Purple
  deriving (Eq, Show, Enum)


-- Function: are these three cards a set?
validSet :: (Card, Card, Card) -> Bool
--validSet Shape
validSet (Card s1 f1 n1 c1, Card s2 f2 n2 c2, Card s3 f3 n3 c3) 
  = (validAttribute s1 s2 s3) && -- valid shapes
    (validAttribute f1 f2 f3) && -- valid fillings
    (validAttribute n1 n2 n3) && -- valid numbers
    (validAttribute c1 c2 c3) -- valid colors

-- check if single attribute is valid in all 3 cards
validAttribute :: Eq a => a -> a -> a -> Bool
validAttribute a1 a2 a3
  | (a1 == a2 && a2 == a3) = True -- all same type
  | (a1 /= a2 && a2 /= a3 && a3 /= a1) = True -- all different type
  | otherwise = False -- not valid set of attributes

-- Does the board contain any sets?
playableBoard :: [Card] -> Bool
playableBoard [] = False
playableBoard cards = checkSets (combinations 3 cards)

-- Check if any of these sets are valid
checkSets :: [[Card]] -> Bool
checkSets [] = False
checkSets (c:cs) =
  case c of
    (c1 : c2 : c3 : []) -> if (validSet (c1, c2, c3)) then True else checkSets cs
    _                   -> False

-- returns list of possible sets
combinations :: Int -> [Card] -> [[Card]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

--updateBoardAndDeck :: (Card, Card, Card) -> [Card] -> [Card] -> ([Card], [Card])
--updateBoardAndDeck set deck board = (deck, board)

updateBoardAndDeck :: (Card, Card, Card) -> [Card] -> [Card] -> IO ([Card], [Card])
updateBoardAndDeck set deck board = do
  let newBoard = removeThree board set -- remove set from board
  deckToBoard deck newBoard -- update board and deck

----              deck      board     deck', board'  (?)  
deckToBoard :: [Card] -> [Card] -> IO ([Card], [Card])
deckToBoard [] board = return ([], board)
deckToBoard deck board = do
  cardsDrawn <- drawCards 3 deck
  let d' = removeList deck cardsDrawn
  let b' = addList board cardsDrawn
  if (not (playableBoard b'))
    then
      deckToBoard d' b'
    else
      return (d', b')

-- add n given cards to board
addList :: [Card] -> [Card] -> [Card]
addList xs     []     = xs
addList []     ys     = ys
addList (x:xs) (y:ys) = x : y : addList xs ys

-- add 3 given cards to board
addThree :: [Card] -> (Card, Card, Card) -> [Card]
addThree cards (c1, c2, c3) = c1 : c2 : c3 : cards

-- remove n given cards from board
removeList :: [Card] -> [Card] -> [Card]
removeList [] _ = []
removeList cards (x:xs) = removeList (removeOne x cards) xs
removeList cards _ = error "tried to remove more than three cards from board"

-- remove 3 given cards from board
removeThree :: [Card] -> (Card, Card, Card) -> [Card]
removeThree [] _ = []
removeThree cards (c1, c2, c3) = removeOne c3 (removeOne c2 (removeOne c1 cards))

-- remove given card if in deck (helper function)
removeOne :: Card -> [Card] -> [Card]
removeOne c cards = if elem c cards then (delete c cards) else error "not in deck"

-- Generate all possible cards
genAll :: [Card]
genAll = do 
  shape <- [Triangle, Squiggle, Oval]
  filling <- [Shaded, Solid, Outline]
  number <- [One, Two, Three]
  color <- [Green, Red, Purple]
  return $ Card shape filling number color

-- pick n randomly
drawCards :: Int -> [a] -> IO [a]
drawCards _ [] = return []
drawCards n cards
    | n < 0 = error "invalid number of cards"
    | otherwise = do 
      pos <- replicateM n $ getStdRandom $ randomR (0, (length cards) - 1)
      return [cards!!p | p <- pos]

-- parser for cards --

spaceParser :: P.Parser String
spaceParser = P.string " " <|> P.string "\n" <|> P.string "\t"

-- white space remover
wsP :: P.Parser a -> P.Parser a
wsP p =  func <$> many spaceParser *> p <* many spaceParser where
  func = pure const

constP :: String -> a -> P.Parser a
constP s x = const x <$> (P.string s)

-- parse shapes
shapeP :: P.Parser Shape
shapeP = constP "Triangle" Triangle <|>
      constP "Squiggle" Squiggle <|>
      constP "Oval" Oval 

-- parse fillings
fillingP :: P.Parser Filling
fillingP = constP "Shaded" Shaded <|>
      constP "Solid" Solid <|>
      constP "Outline" Outline 

-- parse numbers
numberP :: P.Parser Number
numberP = constP "One" One <|>
      constP "Two" Two <|>
      constP "Three" Three 

-- parse colors
colorP :: P.Parser Color
colorP = constP "Green" Green <|>
      constP "Red" Red <|>
      constP "Purple" Purple 

-- parse card
cardP :: P.Parser Card
cardP = pure Card <* wsP(P.string "Card") <*> wsP(shapeP) <*> wsP(fillingP) <*> wsP(numberP) <*> wsP(colorP)

-- parse three card input
parseCards :: P.Parser (Card, Card, Card)
parseCards = pure (,,) <*> wsP(cardP) <* wsP(P.string ",") <*> wsP(cardP) <* wsP(P.string ",") <*> wsP(cardP)

-- parse 12 card input
parseBoard :: P.Parser [Card]
parseBoard = wsP(cardP) `P.sepBy` wsP(P.string ",") 

-- possibly redo this later
isSet :: String -> Bool
isSet input = countLetters input 'C' == 3 

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

---- printing the board
printBoard :: [Card] -> IO ()
printBoard [] = putStrLn "-----"
printBoard (x:xs) = do 
  print x
  printBoard xs

---- printing the set
printSet:: (Card, Card, Card) -> IO ()
printSet (x,y,z) = do 
  print x
  print y
  print z

---------------------------------- THE GAME ---------------------------------------

createGame :: Handle -> Chan (Int, String) -> Bool -> IO ()
createGame handle chan isServer = withSocketsDo $ do
    putStrLn "Created a new game."
    board <- drawCards 12 genAll
    let deck = removeList genAll board
    when isServer (hPrint handle (show (board)))
    mainLoop handle chan isServer deck board


mainLoop :: Handle -> Chan (Int, String) -> Bool -> [Card] -> [Card] -> IO ()
mainLoop handle chan isServer deck board = withSocketsDo $ do
    if (not (playableBoard board) && null deck)
      then do 
        putStrLn "The game has ended"
        hPutStrLn handle ("The game has ended")
      else do
        (src, input) <- readChan chan
        if input == "exit"
          then do
            putStrLn "You or the other player quit.\n"
            return ()
          else do
            -- CASE: USER INPUT
            if src == 0 -- 0 means stdin, 1 means a network message (fix this later)
            then do
              -- validates whether the user's input was a valid set, and if so sends a network msg
              if (validSet (P.getParse parseCards input)) -- check if input is valid
                then do
                  hPutStrLn handle (input) -- ** sends the set over the network
                  -- UPDATE BOARD AND DECK
                  (deck', board') <- updateBoardAndDeck (P.getParse parseCards input) deck board
                  if (isServer) 
                    then do
                      hPutStrLn handle (show (board')) -- ** sends the new board -> client
                      putStr (show (board')) -- ** tells the server (locally) what the new board is
                      mainLoop handle chan isServer deck' board' 
                    else do
                      putStr "nice! you got a set." -- ** just waits for server to send new board
                      mainLoop handle chan isServer deck' board' 
                else do
                  putStrLn "Not a valid set!"
                  mainLoop handle chan isServer deck board 
            ---- CASE: NETWORK MSG
            else do 
              if (isSet input) -- If it's a set
                then do -- UPDATE BOARD AND DECK
                  (deck', board') <- updateBoardAndDeck (P.getParse parseCards input) deck board
                  putStr "Other player found the set: "
                  putStrLn input -- ** print the set that was played
                  if (isServer)
                    then do
                      hPutStrLn handle (show (board')) -- ** sends the new board to client (!)
                      putStr (show (board')) -- ** tells the server (locally) what the new board is
                      mainLoop handle chan isServer deck' board' 
                    else do
                      putStr "the other player got a set" -- ** just waits for server to send new board (NOTE: race condition)
                      mainLoop handle chan isServer deck board 
                else do -- else its a board (client receiving)
                  let incomingBoard = P.getParse parseBoard input
                  putStr input -- ** tells the client (locally) what the incoming board is
                  mainLoop handle chan isServer deck board 
