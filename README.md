# CIS 552 Final Project
### Carolina Zheng (carzheng) and Karinna Loo (kloo)

For our final project, we built a networked version of the game Set that can be played in Terminal. We completed single player and two-player versions of the game. The main components of our project are:

* `Logic.hs`: Contains all the game logic and parsing functions
* `MultiplayerGame.hs`/`SinglePlayerGame.hs`: Contains the main game loops
* `Main.hs`: Sets up the networking (for two-player only) and concurrency before invoking gameplay functions
* Test modules:
	* `Tests.hs`: Contains unit and QuickCheck tests for individual game logic functions
	* `TestSinglePlayer.hs`: Runs a single player game to completion
	* `TestClient.hs`: A client that will connect to a server and automatically play the game, printing the game state to the console as well as running validation tests after every turn

We didn't use any external libraries, but we did use modified versions of the `Parser.hs` and `ParserCombinators.hs` files from class.

#### To run our game
Multiplayer:
~~~~
make
./Main
~~~~

Single player:
~~~~
ghci SinglePlayerGame.hs
> createGame
~~~~
