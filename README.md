# Stones and Rivers 

## Project Specification

**Game**: Stones & Rivers, *by Trevor Harron*

**Group**: Stones & Rivers_4

**Collaborators**:
- João Filipe Oliveira Ramos (up202108743)
- Sara da Silva Azevedo (202006902)

**Contributions**:
- João Ramos: 50%
- Sara Azevedo: 50%


*Identification of the topic (game) and group (group designation, student number and full name of each
member of the group), as well as an indication of the contribution (in percentages, adding up to 100%)
of each member of the group to the assignment;*


## Installation & Execution

To run the game, we need to install SICStus Prolog 4.8 and then change his font to 7. After that, we just must consult the path were the game is (game.pl) and then play. 


*Include all the necessary steps for the correct execution of the game in both
Linux and Windows environments (in addition to the installation of SICStus Prolog 4.8).*

## Game Description

Stones and Rivers is a 2 player strategy game. The goal is to be the first player to get five of the fourteen double-sided pieces into the opponent's score area with the stone side facing up. 

Each player has fourteen pieces, and each piece has two sides, a stone side that have limited movement on their own but block player’s pieces and a river side for creating sweeping movement across the board or pushing obstacles away. 

The players will decide which one is the circle player and which is the square player and then who starts first. After that, they must choose what movement to do with their piece: 

- Flip a piece and rotate - the player can flip both stones and rivers; if the flipped piece is now a river, you can choose the direction up/down or left/right.

- Rotate a river - rotate a river ninety degrees (up/down to left/right or left/right to up/down) and align it along the grid. 

- Move a piece - Both stones and rivers can be moved. Pieces must be placed on the intersections of the grid. Pieces can only move up, down, left, or right. No diagonals or hopping. A piece cannot end its turn on another piece or move off the board. You cannot move in or through the other player’s score area.

- River Movement - If a piece moves onto a River, it can move any number of spaces in the river’s indicated directions until it encounters another piece.

- Pushing with a river - When moving a river, when you would land on a piece you can choose to push it in any number of spaces in one the indicated directions.

If one or more pieces are a river in your score area, the game continues until all 5 pieces in your scoring area are stones. Pieces in a score area can be moved, flipped or rotated. 
The game ends when a player gets five stone pieces in their scoring area. 

Links used to gather information: 

- https://static1.squarespace.com/static/5700bc15e321408302cf9af0/t/646678cb92b5d632c7bb4528/1684437197944/stones+and+rivers+rules.pdf

- https://static1.squarespace.com/static/5700bc15e321408302cf9af0/t/6406b9cc4a4bd62500066c76/1678162382902/stones+and+rivers+official+rules+-+full+art.pdf

- https://bigblueheron.itch.io/stones-and-rivers

*A brief description of the game and its rules (up to 350 words); you should also
include the links used to gather information (official game website, rule book, etc.)*

## Game Logic

*Describe (merely copying the source code is not enough) the design and implementation
of the game logic in Prolog. The starting predicate must be play/0. This section should have information
on the following topics (up to 2000 words in total):
o Internal Game State Representation: describe how the game state is represented, including board
(typically using list of lists with different atoms for the pieces), current player, and possibly captured
and/or pieces yet to be played, or other information that may be required, depending on the game.
It should include examples of the Prolog representation of initial, intermediate and final game
states, and an indication of the meaning of each atom (i.e. how different pieces are represented).
o Game State Visualization: description of the game state display predicate implementation. It
should include information about the created menu system, as well as interaction with the user,
including input validation. The display predicate should be called display_game(+GameState),
receiving the current state of the game and the player who will make the next move. Appealing and
intuitive visualizations will be valued. Flexible game state represen
tations and visualization
predicates will also be valued, for instance those that work with any board size, using an
initial_state(+Size, -GameState) predicate that receives the board size as an argument and returns
an initial game state.
o Move Validation and Execution: describe how a play is validated and executed, obtaining a new
game state. The predicate responsible for move validation and execution should be called
move(+GameState, +Move, -NewGameState).
o List of Valid Moves: describe how to obtain a list of possible moves. The predicate should be named
valid_moves(+GameState, +Player, -ListOfMoves).
o End of Game: verification of the end of the game, with identification of the winner. The predicate
should be called game_over(+GameState, -Winner).
o Game State Evaluation: describe how to evaluate the game state. The predicate should be called
value(+GameState, +Player, -Value).
o Computer Plays: describe how the computer chooses a move, depending on the level of difficulty.
The predicate should be called choose_move(+GameState, +Player, +Level, -Move). Level 1 should
return a valid random move. Level 2 should return the best play at the time (using a greedy
algorithm), considering the evaluation of the game state, as described above.*

## Conclusions

*Conclusions about the work carried out, including limitations of the program (known
issues), as well as possible improvements (roadmap) (up to 250 words);*

## Bibliography

- https://static1.squarespace.com/static/5700bc15e321408302cf9af0/t/6406b9cc4a4bd62500066c76/1678162382902/stones+and+rivers+official+rules+-+full+art.pdf
- https://static1.squarespace.com/static/5700bc15e321408302cf9af0/t/646678cb92b5d632c7bb4528/1684437197944/stones+and+rivers+rules.pdf
- https://bigblueheron.itch.io/stones-and-rivers

*List of books, papers, web pages and other resources used during the development of
the assignment.*

## Extra

*You can also include one or more images illustrating the execution of the game, showing initial, intermediate
and final game states (these game states can be hard-coded directly into the code file for this demonstration
of the game state visualization, using predicates similar to the initial_state/2 predicate).*
