# CIS 552 Final Project
### Karinna Loo and Carolina Zheng

Carolina Zheng (carzheng)
Karinna Loo (kloo)
For our final project we built a networked version of the game Set. We completed single player and two-player versions of the game. The main components of our project include:

Logic.hs: contains all the game logic and parsing functions
Main.hs: that sets up the networking (for two-player only) and concurrency
MultiplayerGame.hs/SinglePlayerGame.hs: contains the main game loops
Test modules:
Tests.hs: contains unit and QuickCheck tests for individual game logic functions
TestSinglePlayer.hs: simply runs a single player game to completion
TestClient.hs: is a client that will connect to a server and automatically play the game, printing the game state to the console as well as running validation tests after every turn.

We didn’t use any external libraries, but we did use the Parser.hs and ParserCombinators.hs files from class.