module SinglePlayerGame where

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
validSet :: Maybe (Card, Card, Card) -> [Card] -> Bool
--validSet Shape
validSet (Just (a@(Card s1 f1 n1 c1), b@(Card s2 f2 n2 c2), c@(Card s3 f3 n3 c3))) board = 
  if ((elem a board) && (elem b (removeOne a board)) && (elem c (removeOne b (removeOne a board))))
    then (validAttribute s1 s2 s3) && -- valid shapes
    (validAttribute f1 f2 f3) && -- valid fillings
    (validAttribute n1 n2 n3) && -- valid numbers
    (validAttribute c1 c2 c3) -- valid colors
    else False
validSet Nothing _ = False

-- check if single attribute is valid in all 3 cards
validAttribute :: Eq a => a -> a -> a -> Bool
validAttribute a1 a2 a3
  | (a1 == a2 && a2 == a3) = True -- all same type
  | (a1 /= a2 && a2 /= a3 && a3 /= a1) = True -- all different type
  | otherwise = False -- not valid set of attributes

-- Does the board contain any sets?
playableBoard :: [Card] -> Bool
playableBoard [] = False
playableBoard cards = checkSets (combinations 3 cards) cards

-- Check if any of these sets are valid
checkSets :: [[Card]] -> [Card] -> Bool
checkSets [] c = False
checkSets (c:cs) cards =
  case c of
    (c1 : c2 : c3 : []) -> if (validSet (Just (c1, c2, c3)) cards) then True else (checkSets cs cards)
    _                   -> False


-- Does the board contain any sets?
getValidSet :: [Card] -> IO ()
getValidSet [] = print "nooo"
getValidSet cards = getValidSet' (combinations 3 cards) cards

-- Get a valid Set
getValidSet' :: [[Card]] -> [Card] -> IO ()
getValidSet' [] c = print "ahhhh"
getValidSet' (c:cs) cards =
  case c of
    (c1 : c2 : c3 : []) -> if (validSet (Just (c1, c2, c3)) cards) 
      then printSet (c1,c2,c3)
      else getValidSet' cs cards 
    _                   -> print "ughhh"

-- returns list of possible sets
combinations :: Int -> [Card] -> [[Card]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

--updateBoardAndDeck :: (Card, Card, Card) -> [Card] -> [Card] -> ([Card], [Card])
--updateBoardAndDeck set deck board = (deck, board)

updateBoardAndDeck :: Maybe (Card, Card, Card) -> [Card] -> [Card] -> IO ([Card], [Card])
updateBoardAndDeck (Just set) deck board = do
  let newBoard = removeThree board set -- remove set from board
  deckToBoard deck newBoard -- update board and deck
updateBoardAndDeck Nothing deck board = deckToBoard deck board

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
-- Board -> Cards to add -> New Board
addList :: [Card] -> [Card] -> [Card]
addList b cs = b ++ cs

-- add 3 given cards to board
addThree :: [Card] -> (Card, Card, Card) -> [Card]
addThree cards (c1, c2, c3) = c1 : c2 : c3 : cards

-- remove n given cards from board
removeList :: [Card] -> [Card] -> [Card]
removeList cards [] = cards
removeList cards (x:xs) = removeList (removeOne x cards) xs
-- removeList cards _ = error "tried to remove more than three cards from board"

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
--        n cards -> deck -> drawn cards
drawCards :: Int -> [Card] -> IO [Card]
drawCards _ [] = return []
drawCards n cards
    | n <= 0 = return []
    | otherwise = do 
        index <- getStdRandom $ randomR (0, (length cards) - 1)
        rest <- (drawCards (n - 1) (removeOne (cards!!index) cards))
        return (cards!!index : rest)

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

createGame :: IO ()
createGame = do
    putStrLn "Created a new game."
    board <- drawCards 12 genAll
    let deck = removeList genAll board
    putStrLn (show board)
    mainLoop deck board


-- wrapper :: P.Parser (Card, Card, Card) -> String -> (Card, Card, Card)
-- wrapper p str -> 

mainLoop :: [Card] -> [Card] -> IO ()
mainLoop deck board = do
    input <- getLine
    if (not (playableBoard board) && null deck)
      then do 
        putStrLn "The game has ended"
      else do
        if input == "exit"
          then do
            putStrLn "You quit.\n"
            return ()
          else do            -- validates whether the user's input was a valid set, and if so sends a network msg
            if (validSet (P.getParse parseCards input) board) -- check if input is valid
              then do
                -- UPDATE BOARD AND DECK
                (deck', board') <- updateBoardAndDeck (P.getParse parseCards input) deck board
                putStrLn "nice! you got a set."
                putStrLn (show (board')) -- ** tells the server (locally) what the new board is
                mainLoop deck' board' 
              else do
                putStrLn "Not a valid set or set not in board!"
                mainLoop deck board
