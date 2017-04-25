module MultiplayerGame where

import Network
import System.IO
import Control.Concurrent

import Control.Applicative (Alternative(..))
import Control.Monad
import Data.List (tails, delete)
import Data.Maybe
import System.Random

import qualified Parser as P
import qualified ParserCombinators as P

-- Networking definitions
type ServerFlag = Bool
type Message = String
data InputSource =
    Stdin
  | Network
  deriving (Eq)

-- Game state definitions
type Deck = [Card]
type Board = [Card]
type Set = (Card, Card, Card)

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

validBoard :: Maybe Board -> Bool
validBoard = isJust

-- Checks if a set is in the board
boardContainsSet :: Set -> Board -> Bool
boardContainsSet (a, b, c) board =
  elem a board && elem b (removeOne a board) && elem c (removeOne b (removeOne a board))

-- Checks if set is playable
playableSet :: Maybe Set -> Board -> Bool
playableSet Nothing _ = False
playableSet (Just set) board = boardContainsSet set board && validSet set

-- Checks if the set is valid according to rules
validSet :: Set -> Bool
validSet (Card s1 f1 n1 c1, Card s2 f2 n2 c2, Card s3 f3 n3 c3) 
  = validAttribute s1 s2 s3 && -- valid shapes
    validAttribute f1 f2 f3 && -- valid fillings
    validAttribute n1 n2 n3 && -- valid numbers
    validAttribute c1 c2 c3    -- valid colors

-- Checks if single attribute is valid in all 3 cards
validAttribute :: Eq a => a -> a -> a -> Bool
validAttribute a1 a2 a3
  | a1 == a2 && a2 == a3 = True -- all same type
  | a1 /= a2 && a2 /= a3 && a3 /= a1 = True -- all different type
  | otherwise = False -- not valid set of attributes

-- Checks if the board contains any sets
playableBoard :: Board -> Bool
playableBoard []    = False
playableBoard cards = checkSets (combinations 3 cards)

-- Check if any of these 'sets' are valid
checkSets :: [[Card]] -> Bool
checkSets []     = False
checkSets (c:cs) =
  case c of
    [c1, c2, c3] -> validSet (c1, c2, c3) || checkSets cs
    _            -> error "Not a set"

-- Tester function: Gets a valid set from board
getValidSet :: Board -> IO ()
getValidSet []    = print "nooo"
getValidSet cards = getValidSet' (combinations 3 cards) cards

-- Get a valid Set
getValidSet' :: [[Card]] -> Board -> IO ()
getValidSet' [] _ = print "ahhhh"
getValidSet' (c:cs) cards =
  case c of
    [c1, c2, c3] -> if validSet (c1, c2, c3)
      then printSet (c1, c2, c3)
      else getValidSet' cs cards 
    _            -> print "ughhh"

-- Returns list of possible sets
combinations :: Int -> Board -> [[Card]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

updateBoardAndDeck :: Set -> Deck -> Board -> IO (Deck, Board)
updateBoardAndDeck set deck board = do
  let newBoard = removeThree board set -- remove set from board
  deckToBoard deck newBoard -- update board and deck

deckToBoard :: Deck -> Board -> IO (Deck, Board)
deckToBoard [] board = return ([], board)
deckToBoard deck board = do
  cardsDrawn <- drawCards 3 deck
  let d' = removeList deck cardsDrawn
  let b' = addList board cardsDrawn
  if not (playableBoard b')
    then
      deckToBoard d' b'
    else
      return (d', b')

-- add n given cards to board
addList :: Board -> [Card] -> Board
addList b cs = b ++ cs

-- add 3 given cards to board
addThree :: Board -> (Card, Card, Card) -> Board
addThree cards (c1, c2, c3) = c1 : c2 : c3 : cards

-- remove n given cards from board
removeList :: Board -> [Card] -> Board
-- removeList cards [] = cards
-- removeList cards (x:xs) = removeList (removeOne x cards) xs
removeList = foldl (flip removeOne)

-- remove 3 given cards from board
removeThree :: Board -> (Card, Card, Card) -> Board
removeThree [] _ = []
removeThree cards (c1, c2, c3) = removeOne c3 (removeOne c2 (removeOne c1 cards))

-- remove given card if in deck (helper function)
removeOne :: Card -> Deck -> Deck
removeOne c cards = if c `elem` cards then delete c cards else error "not in deck"

-- Generate all possible cards
genAll :: [Card]
genAll = do
  shape   <- [Triangle, Squiggle, Oval]
  filling <- [Shaded, Solid, Outline]
  number  <- [One, Two, Three]
  color   <- [Green, Red, Purple]
  return $ Card shape filling number color

-- pick n cards randomly
drawCards :: Int -> Deck -> IO [Card]
drawCards _ [] = return []
drawCards n cards
    | n <= 0 = return []
    | otherwise = do 
        index <- getStdRandom $ randomR (0, length cards - 1)
        rest  <- drawCards (n - 1) (removeOne (cards!!index) cards)
        return (cards!!index : rest)

-- parser for cards --

spaceParser :: P.Parser String
spaceParser = P.string " " <|> P.string "\n" <|> P.string "\t"

-- white space remover
wsP :: P.Parser a -> P.Parser a
wsP p =  func <$> many spaceParser *> p <* many spaceParser where
  func = pure const

constP :: String -> a -> P.Parser a
constP s x = const x <$> P.string s

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
cardP = pure Card <* wsP(P.string "Card") <*> wsP shapeP <*> wsP fillingP <*> wsP numberP <*> wsP colorP

-- parse three card input
parseCards :: P.Parser Set
parseCards = pure (,,) <*> wsP cardP <* wsP(P.string ",") <*> wsP cardP <* wsP(P.string ",") <*> wsP cardP

-- parse 12 card input
parseBoard :: P.Parser Board
parseBoard = wsP(P.string "[") *> (wsP cardP `P.sepBy` wsP(P.string ",")) <* wsP(P.string "]")

-- possibly redo this later
isSet :: String -> Bool
isSet input = countLetters input 'C' == 3 

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

---- printing the board
printBoard :: Board -> IO ()
printBoard [] = putStrLn "-----"
printBoard (x:xs) = do 
  print x
  printBoard xs

---- printing the set
printSet :: Set -> IO ()
printSet (x, y, z) = do 
  putStr (show x)
  putStr ","
  putStr (show y)
  putStr ","
  print z

displayBoard :: Board -> IO ()
displayBoard b = do
                   putStrLn "Board: "
                   print b

---------------------------------- THE GAME ---------------------------------------

createGame :: Handle -> Chan (InputSource, Message) -> ServerFlag -> IO ()
createGame handle chan isServer = withSocketsDo $ do
    putStrLn "Created a new game."
    board <- drawCards 12 genAll
    let deck = removeList genAll board
    when isServer (hPrint handle board >> displayBoard board)
    mainLoop handle chan isServer deck board


mainLoop :: Handle -> Chan (InputSource, Message) -> ServerFlag -> Deck -> Board -> IO ()
mainLoop handle chan isServer deck board = withSocketsDo $
    if not (playableBoard board) && null deck
      then do
        putStrLn "The game has ended."
        hPutStrLn handle "The game has ended."
      else do
        (src, input) <- readChan chan
        if input == "exit"
          then putStrLn "You or the other player quit.\n"
          else playTurn handle chan isServer deck board src input


playTurn :: Handle -> Chan (InputSource, Message) -> ServerFlag ->
            Deck -> Board -> InputSource -> Message -> IO ()
playTurn handle chan isServer deck board src input = withSocketsDo $
    if src == Stdin
    then
      -- validates whether the user's input was a valid set, and if so sends a network msg
      if playableSet (P.getParse parseCards input) board
        then do
          putStrLn "Nice! You got a set."
          hPutStrLn handle input
          if isServer
            then serverUpdateGameState
            else updateGameState board
        else do
          putStrLn "Not a valid set or set not in board!"
          updateGameState board
    else if src == Network then
      if isSet input -- Other player sent a valid set
        then do
          putStr "Other player found the set: "
          putStrLn input
          if isServer
            then serverUpdateGameState
            else updateGameState board
        else do -- Server sent a new board to the client
          let mBoard = P.getParse parseBoard input
          if validBoard mBoard
            then do
              let board' = fromJust mBoard
              displayBoard board'
              updateGameState board'
            else
              putStrLn "Received invalid board from server."
    else
      error "Unknown input source"
    where serverUpdateGameState = do
              (deck', board') <- updateBoardAndDeck (fromJust $ P.getParse parseCards input)
                                                     deck board
              hPrint handle board' -- ** sends the new board to client
              displayBoard board'
              mainLoop handle chan isServer deck' board'
          updateGameState = mainLoop handle chan isServer deck
