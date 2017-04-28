module Logic where

import Control.Applicative (Alternative(..))
import Data.List (tails, delete)
import Data.Maybe (isJust)
import System.Random

import qualified Parser as P
import qualified ParserCombinators as P

------------------- Definitions (Game State) -------------------

type Deck  = [Card]
type Board = [Card]
type Set   = (Card, Card, Card)

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


------------------- GUI to print board nicely -------------------

prettyShowBoard :: Board -> String
prettyShowBoard board =
    showHelper board (1 :: Int) (length board)
    where
      showHelper (c : cs) num boardLength
        | num > boardLength    = ""
        | num `mod` 4 == 0 &&
          num < boardLength    = show num ++ pshow c ++ "\n" ++ showHelper cs (num + 1) boardLength
        | otherwise            = (if num `mod` 4 == 1 && num < 10 then " " else "") ++
                                 show num ++ pshow c ++
                                 (if num < 9 then "   " else "  ") ++
                                 showHelper cs (num + 1) boardLength
      showHelper [] _ _        = ""

prettyShowSet :: Set -> String
prettyShowSet (c1, c2, c3) = pshow c1 ++ "\n" ++ pshow c2 ++ "\n" ++ pshow c3 ++ "\n"

class PrettyShow a where
    pshow :: a -> String

instance PrettyShow Card where
  pshow (Card s f n c) = "[" ++ pshow s ++ " " ++ pshow f ++ " " ++ pshow n ++ " " ++ pshow c ++ "]"

instance PrettyShow Shape where
  pshow Triangle = "A"
  pshow Squiggle = "B"
  pshow Oval     = "C"

instance PrettyShow Filling where
  pshow Shaded  = "X"
  pshow Solid   = "O"
  pshow Outline = "V"

instance PrettyShow Number where
  pshow One   = "1"
  pshow Two   = "2"
  pshow Three = "3"

instance PrettyShow Color where
  pshow Green  = "@"
  pshow Red    = "#"
  pshow Purple = "$"


------------------- Game Logic Functions -------------------

-- Gets the cards corresponding to given ints
getCards :: Board -> Maybe (Int, Int, Int) -> Maybe Set
getCards board (Just (x,y,z)) = Just (board!!(x-1), board!!(y-1), board!!(z-1))
getCards _ Nothing            = Nothing

removePunc :: Maybe Set -> String
removePunc Nothing    = ""
removePunc (Just set) = [ x | x <- show set, x `notElem` "()" ]

-- Checks if a set is in the board (note that no duplicates are allowed)
boardContainsSet :: Set -> Board -> Bool
boardContainsSet (a, b, c) board =
  elem a board && elem b board && elem c board && a /= b && a /= c && b /= c

-- Checks if set is playable
playableSet :: Maybe Set -> Board -> Bool
playableSet Nothing _        = False
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
  | a1 == a2 && a2 == a3             = True -- all same type
  | a1 /= a2 && a2 /= a3 && a3 /= a1 = True -- all different type
  | otherwise                        = False -- not valid set of attributes

-- Checks if the board contains any sets
playableBoard :: Board -> Bool
playableBoard []    = False
playableBoard cards = checkSets (combinations 3 cards)

-- Check if any of these sets are valid
checkSets :: [[Card]] -> Bool
checkSets []     = False
checkSets (c:cs) =
  case c of
    [c1, c2, c3] -> validSet (c1, c2, c3) || checkSets cs
    _            -> error "Not a set"

-- Returns list of possible sets
combinations :: Int -> Board -> [[Card]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

-- Adds three cards to board (called when board has no sets)
deckToBoard :: Deck -> Board -> IO (Deck, Board)
deckToBoard [] board   = return ([], board)
deckToBoard deck board = do
  cardsDrawn <- drawCards (if length deck >= 3 then 3 else length deck) deck
  let d' = removeList deck cardsDrawn
  let b' = addList board cardsDrawn
  if not (playableBoard b') && not (null deck)
    then deckToBoard d' b'
    else return (d', b')

-- Replaces the first card with the second at the same index in the board
replaceCard :: Card -> Card -> Board -> Board
replaceCard oldCard newCard board =
  case board of
    (c : cs) -> if c == oldCard then newCard : cs else c : replaceCard oldCard newCard cs
    []       -> error "Card to be replaced not in board"

setToList :: Set -> [Card]
setToList (c1, c2, c3) = [c1, c2, c3]

tuplify :: [a] -> (a,a,a)
tuplify [x,y,z] = (x,y,z)
tuplify _ = error "Invalid number of cards in list"

-- Given a list of cards to remove, updates board and ensures it's playable
updateBoardAndDeck :: [Card] -> Deck -> Board -> IO (Deck, Board)
updateBoardAndDeck (c : cs) deck board
  | null deck || length board > 12 = do
                                       let b' = removeOne c board
                                       updateBoardAndDeck cs deck b'
  | otherwise                      = do
                                       newCardList <- drawCards 1 deck
                                       let newCard  = head newCardList
                                       let d'       = removeOne newCard deck
                                       let b'       = replaceCard c newCard board
                                       updateBoardAndDeck cs d' b'
updateBoardAndDeck []       deck board =
  if not (playableBoard board) && not (null deck) -- Not playable, so add 3 cards to board
    then deckToBoard deck board
    else return (deck, board)

-- add n given cards to board
addList :: Board -> [Card] -> Board
addList b cs = b ++ cs

-- add 3 given cards to board
addThree :: Board -> (Card, Card, Card) -> Board
addThree cards (c1, c2, c3) = c1 : c2 : c3 : cards

-- remove n given cards from board
removeList :: Board -> [Card] -> Board
removeList = foldl (flip removeOne)

-- remove 3 given cards from board
removeThree :: Board -> (Card, Card, Card) -> Board
removeThree [] _               = []
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
drawCards _ []  = return []
drawCards n cards
    | n <= 0    = return []
    | otherwise = do 
                    index <- getStdRandom $ randomR (0, length cards - 1)
                    rest  <- drawCards (n - 1) (removeOne (cards!!index) cards)
                    return (cards!!index : rest)

-- Checks if the network message is parsable as a set
isSet :: String -> Bool
isSet input = isJust $ P.getParse parseCards input

-- Checks if the network message is parsable as a board
isBoard :: String -> Bool
isBoard input = isJust $ P.getParse parseBoard input

-- Prints the board nicely to terminal
displayBoard :: Board -> IO ()
displayBoard b = do
                   putStrLn "\n ~~~~~~~~~~~~~~~~~~~[  Board  ]~~~~~~~~~~~~~~~~~~~"
                   putStrLn (prettyShowBoard b) -- This line prints the board with a GUI
                   putStrLn " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                   --print b                      -- This line prints the board as is, for debug
                   putStrLn ""


------------------- Parsing -------------------

spaceParser :: P.Parser String
spaceParser = P.string " " <|> P.string "\n" <|> P.string "\t"

-- white space remover
wsP :: P.Parser a -> P.Parser a
wsP p = func <$> many spaceParser *> p <* many spaceParser
        where func = pure const

constP :: String -> a -> P.Parser a
constP s x = const x <$> P.string s

-- parse shapes
shapeP :: P.Parser Shape
shapeP = constP "Triangle" Triangle <|>
         constP "Squiggle" Squiggle <|>
         constP "Oval"     Oval 

-- parse fillings
fillingP :: P.Parser Filling
fillingP = constP "Shaded"  Shaded   <|>
           constP "Solid"   Solid    <|>
           constP "Outline" Outline 

-- parse numbers
numberP :: P.Parser Number
numberP = constP "One"   One    <|>
          constP "Two"   Two    <|>
          constP "Three" Three 

-- parse colors
colorP :: P.Parser Color
colorP = constP "Green"  Green   <|>
         constP "Red"    Red     <|>
         constP "Purple" Purple 

-- parse input number
parseInP :: P.Parser (Int,Int,Int)
parseInP =  pure (,,) <*> wsP P.int <* wsP(P.string ",")
                      <*> wsP P.int <* wsP(P.string ",") <*> wsP P.int

-- parse card
cardP :: P.Parser Card
cardP = pure Card <* wsP(P.string "Card") <*> wsP shapeP <*> wsP fillingP
                                          <*> wsP numberP <*> wsP colorP

-- parse three card input
parseCards :: P.Parser Set
parseCards = pure (,,) <*> wsP cardP <* wsP(P.string ",")
                       <*> wsP cardP <* wsP(P.string ",") <*> wsP cardP

-- parse 12 card input
parseBoard :: P.Parser Board
parseBoard = wsP(P.string "[") *> (wsP cardP `P.sepBy` wsP(P.string ",")) <* wsP(P.string "]")


----------------- Debugging/Testing -------------------

-- Finds a valid set from board
retrieveValidSet :: Board -> Maybe Set
retrieveValidSet []    = Nothing
retrieveValidSet cards = retrieveValidSet' (combinations 3 cards) cards

-- Helper for above
retrieveValidSet' :: [[Card]] -> Board -> Maybe Set
retrieveValidSet' ([c1, c2, c3] : cs) cards =
    if validSet (c1, c2, c3)
      then Just (c1, c2, c3)
      else retrieveValidSet' cs cards 
retrieveValidSet' _ _ = Nothing

-- Finds a valid set from board
getValidSet :: Board -> IO ()
getValidSet []    = error "Inconsistent board state"
getValidSet cards = getValidSet' (combinations 3 cards) cards

-- Helper for above
getValidSet' :: [[Card]] -> Board -> IO ()
getValidSet' ([c1, c2, c3] : cs) cards =
    if validSet (c1, c2, c3)
      then printSet (c1, c2, c3)
      else getValidSet' cs cards 
getValidSet' _ _ = error "Inconsistent board state"

---- Printing the board
printBoard :: Board -> IO ()
printBoard [] = putStrLn "-----"
printBoard (x:xs) = do 
  print x
  printBoard xs

---- Printing the set
printSet :: Set -> IO ()
printSet (x, y, z) = do 
  putStr (show x)
  putStr ","
  putStr (show y)
  putStr ","
  print z
