:- ensure_loaded('draw.pl').
:- use_module(library(random)).
:- use_module(library(lists)).

% # Play

turn(player_a, 0, 'Player A turn.\n').
turn(player_b, 1, 'Player B turn.\n').

decide_first_turn(T) :-
    random(0, 2, R),
    turn(T, R, _).

swicth_turn(player_a, player_b).
swicth_turn(player_b, player_a).

piece_in(X, Y, Board, Piece) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece).

validate_piece(Turn, Piece):- belongs_to(Piece, Turn), !.
validate_piece(Turn, Piece):-
    \+ belongs_to(Piece, Turn),
    repeat,
    write('Invalid! Provive a valid X-Y\n'),
    read(X-Y),
    board(Board),
    piece_in(X, Y, Board, Piece1),
    belongs_to(Piece1, Turn).

flip(squareStn, horizontal, squareHrz) :- !.
flip(squareStn, veritcal, squareVrt) :- !.
flip(circleStn, horizontal, circleHrz) :- !.
flip(circleStn, veritcal, circleVrt) :- !.
flip(squareHrz, _, squareStn) :- !.
flip(squareVrt, _, squareStn) :- !.
flip(circleHrz, _, circleStn) :- !.
flip(circleVrt, _, circleStn) :- !.

move_option(1, move).
move_option(2, flip).

select_stone_move(Move) :-
    write('Select a move for the '),
    write(' stone:\n'),
    write('1 - Move\n'),
    write('2 - Flip\n'),
    read(Option),
    move_option(Option, Move).

select_move(squareStn, Move) :- select_stone_move(Move).
select_move(circleStn, Move) :- select_stone_move(Move).

execute_move(move, X, Y, _Piece) :-
    write('Provide the coordinates of the destination, as follows: X-Y\n'),
    read(X1-Y1),
    board(Board),
    board_size(Board, W, H),
    X1 < W, X1 >= 0, 
    Y1 < H, Y1 >= 0,
    %only ortogonal moves
    (X1 =:= X, Y1 =:= Y + 1; X1 =:= X, Y1 =:= Y - 1; X1 =:= X + 1, Y1 =:= Y; X1 =:= X - 1, Y1 =:= Y),
    piece_in(X1, Y1, Board, emptySlot),
    write('Valid Move!\n').

execute_move(flip, X, Y, Piece) :-
    flip(Piece, _, NewPiece),
    board(Board),
    nth0(Y, Board, Row, RestBoard),
    nth0(X, Row, Piece, RestRow), 
    nth0(X, NewRow, NewPiece, RestRow),
    nth0(Y, NewBoard, NewRow, RestBoard),
    retract(board(Board) :- !),
    asserta(board(NewBoard) :- !).

game_loop(Turn) :-
    board(B),
    draw(B),
    turn(Turn, _, TurnText),
    write(TurnText),
    write('Provide the coordinates of the piece you wish to play, as follows: X-Y\n'),
    read(X-Y),
    piece_in(X, Y, B, Piece),
    validate_piece(Turn, Piece),
    select_move(Piece, Move),
    execute_move(Move, X, Y, Piece),
    swicth_turn(Turn, NewTurn), 
    game_loop(NewTurn).

play :- 
    decide_first_turn(T),
    game_loop(T).
    



    
