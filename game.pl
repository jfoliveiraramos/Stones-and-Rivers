:- ensure_loaded('draw.pl').
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).

% # Play

turn(player_a, 0, 'Player A turn.\n').
turn(player_b, 1, 'Player B turn.\n').

decide_first_turn(T) :-
    random(0, 2, R),
    turn(T, R, _).

swicth_turn(player_a, player_b).
swicth_turn(player_b, player_a).

piece_in(Piece, X-Y) :-
    board(Board),
    nth0(Y, Board, Row),
    nth0(X, Row, Piece).

validate_piece(X-Y, Turn, Piece):- 
    piece_in(Piece, X-Y),
    belongs_to(Piece, Turn). 

get_piece(Turn, Piece, X, Y) :-
    write('Provide the coordinates of the piece you wish to play, as follows: X-Y\n'),
    read(X-Y),
    validate_piece(X-Y, Turn, Piece), !.

get_piece(Turn, Piece, X, Y) :-
    repeat,
    write('Invalid! Provide a valid piece\n'),
    read(X-Y),
    validate_piece(X-Y, Turn, Piece).

replace_piece(X, Y, NewPiece) :-
    board(Board),
    nth0(Y, Board, Row, RestBoard),
    nth0(X, Row, _, RestRow), 
    nth0(X, NewRow, NewPiece, RestRow),
    nth0(Y, NewBoard, NewRow, RestBoard),
    retract((board(Board) :- !)),
    asserta((board(NewBoard) :- !)).

flip(squareStn, horizontal, squareHrz) :- !.
flip(squareStn, vertical, squareVrt) :- !.
flip(circleStn, horizontal, circleHrz) :- !.
flip(circleStn, vertical, circleVrt) :- !.
flip(squareHrz, squareStn) :- !.
flip(squareVrt, squareStn) :- !.
flip(circleHrz, circleStn) :- !.
flip(circleVrt, circleStn) :- !.

rotate(squareVrt, squareHrz) :- !.
rotate(squareHrz, squareVrt) :- !.
rotate(circleVrt, circleHrz) :- !.
rotate(circleHrz, circleVrt) :- !.

play_option(1, move).
play_option(2, flip).
play_option(3, rotate).

validate_option(Option, L-U) :- between(L, U, Option).

select_stone_play(Move) :-
    write('1 - Move\n'),
    write('2 - Flip\n'),
    write('Select a move for the stone\n'),
    read(Option),
    validate_option(Option, 1-2),
    play_option(Option, Move), 
    !.

select_stone_play(Move) :-
    repeat,
    write('Invalid! Provide a valid option\n'),
    read(Option),
    validate_option(Option, 1-2),
    play_option(Option, Move).

select_river_play(Move) :-
    write('1 - Move\n'),
    write('2 - Flip\n'),
    write('3 - Rotate\n'),
    write('Select a move for the river\n'),
    read(Option),
    validate_option(Option, 1-3),
    play_option(Option, Move), 
    !.

select_river_play(Move) :-
    repeat,
    write('Invalid! Provide a valid option\n'),
    read(Option),
    validate_option(Option, 1-3),
    play_option(Option, Move).

select_play(squareStn, Move) :- !, select_stone_play(Move).
select_play(circleStn, Move) :- !, select_stone_play(Move).

select_play(squareVrt, Move) :- !, select_river_play(Move).
select_play(squareHrz, Move) :- !, select_river_play(Move).
select_play(circleVrt, Move) :- !, select_river_play(Move).
select_play(circleHrz, Move) :- !, select_river_play(Move).

get_direction(1, horizontal) :- !.
get_direction(2, vertical) :- !.    

validate_move(X, Y, X1, Y1) :- 
    board(Board),
    board_size(Board, W, H),
    X1 < W, X1 >= 0, 
    Y1 < H, Y1 >= 0,
    (X1 =:= X, Y1 =:= Y + 1; X1 =:= X, Y1 =:= Y - 1; X1 =:= X + 1, Y1 =:= Y; X1 =:= X - 1, Y1 =:= Y),
    piece_in(emptySlot, X1-Y1).

get_move(X, Y, X1, Y1) :-
    write('Provide the coordinates of the move, as follows: X-Y\n'),
    read(X1-Y1),
    validate_move(X, Y, X1, Y1),
    !.
    
get_move(X, Y, X1, Y1) :- 
    repeat,
    write('Invalid! Provide a valid X-Y\n'),
    read(X1-Y1),
    validate_move(X, Y, X1, Y1).

execute_play(move, X, Y, Piece) :-
    get_move(X, Y, X1, Y1),
    replace_piece(X, Y, emptySlot),
    replace_piece(X1, Y1, Piece).

execute_play(flip, X, Y, Piece) :-
    (Piece = squareStn; Piece = circleStn),
    write('1 - Horizontal\n'),
    write('2 - Vertical\n'),
    write('Provide the direction of the flip\n'),
    read(Option),
    validate_option(Option, 1-2),
    get_direction(Option, Direction),
    flip(Piece, Direction, Flipped),
    replace_piece(X, Y, Flipped), !.

execute_play(flip, X, Y, Piece) :-
    (Piece = squareStn; Piece = circleStn), !,
    repeat,
    write('Invalid! Provide a valid direction\n'),
    read(Option),
    validate_option(Option, 1-2),
    get_direction(Option, Direction),
    flip(Piece, Direction, Flipped),
    replace_piece(X, Y, Flipped).

execute_play(flip, X, Y, Piece) :-
    !,
    flip(Piece, Flipped),
    replace_piece(X, Y, Flipped).

execute_play(rotate, X, Y, Piece) :-
    !,
    rotate(Piece, Rotated),
    replace_piece(X, Y, Rotated).

play_turn(Turn) :-
    turn(Turn, _, TurnText),
    write(TurnText),
    get_piece(Turn, Piece, X, Y),
    select_play(Piece, Move),
    execute_play(Move, X, Y, Piece).

game_loop(Turn) :-
    repeat,
    board(B),
    draw(B),
    play_turn(Turn),
    swicth_turn(Turn, NewTurn), 
    game_loop(NewTurn).

play :- 
    decide_first_turn(T),
    game_loop(T).
    



    
