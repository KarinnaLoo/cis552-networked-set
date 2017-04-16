{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative (Alternative(..))
import Control.Monad ()

import Data.List (tails, delete)

import Data.List.Split

import System.Random

import Control.Monad (replicateM)

import qualified Parser as P
import qualified ParserCombinators as P
-------------------------------------------------------------------------
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
validSet :: Card -> Card -> Card -> Bool
--validSet Shape
validSet (Card s1 f1 n1 c1) 
         (Card s2 f2 n2 c2) 
         (Card s3 f3 n3 c3) = (validAttribute s1 s2 s3) && -- valid shapes
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
    (c1 : c2 : c3 : []) -> if (validSet c1 c2 c3) then True else checkSets cs
    _                   -> False

-- returns list of possible sets
combinations :: Int -> [Card] -> [[Card]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

-- add 3 given cards to board
addThree :: [Card] -> Card -> Card -> Card -> [Card]
addThree deck c1 c2 c3 = c1 : c2 : c3 : deck

-- add 3 given cards to board
addThree :: [Card] -> (Card, Card, Card) -> [Card]
addThree cards (c1, c2, c3) = c1 : c2 : c3 : cards

-- remove 3 given cards from board
removeListThree :: [Card] -> [Card]-> [Card]
removeListThree [] _ = []
removeListThree cards (c1:c2:c3:[]) = removeOne c3 (removeOne c2 (removeOne c1 cards))
removeListThree cards _ = error "tried to remove more than three cards from board"

-- remove 3 given cards from board
removeThree :: [Card] -> Card -> Card -> Card -> [Card]
removeThree [] c1 c2 c3 = []
removeThree cards c1 c2 c3 = removeOne c3 (removeOne c2 (removeOne c1 cards))

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

-- pick n randomly TODO
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

---- printing the board
printBoard :: [Card] -> IO ()
printBoard [] = putStrLn "-----"
printBoard (x:xs) = do 
  print x
  printBoard xs

main :: IO ()
main = do 
  getLine >>= (\x -> putStrLn x)
  main

-- Notes --

-- Main game loop
  -- generate list of possible cards, pick 12 randomly -> board, rest -> deck
  -- function loop: ask the player which three cards want to try, IO input, parse into cards
    -- if valid, remove from board & add +1 to their score
      -- if can, replace with 3 more cards (*)
        -- check if board still playable
          -- if is, display again
        -- else (*)
      -- else no more cards, display winner
    -- else, print "not a valid set"


