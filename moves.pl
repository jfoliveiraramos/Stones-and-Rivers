:- use_module(library(between)).

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

% -----------------------------------------------------


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

% -----------------------------------------------------

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

% -----------------------------------------------------

free_adjacent(horizontal, X-Y-Piece):-
    X1 is X + 1,
    piece_in(Target1, X1-Y),
    can_move_over(Piece, X1-Y-Target1, Out1), Out1 \= pushMove,
    !.
free_adjacent(horizontal, X-Y-Piece):-
    X2 is X - 1,
    piece_in(Target2, X2-Y),
    can_move_over(Piece, X2-Y-Target2, Out2), Out2 \= pushMove,
    !.
    
free_adjacent(vertical, X-Y-Piece):-
    Y1 is Y + 1,
    piece_in(Target1, X-Y1),
    can_move_over(Piece, X-Y1-Target1, Out1), Out1 \= pushMove,
    !.
free_adjacent(vertical, X-Y-Piece):-
    Y2 is Y - 1,
    piece_in(Target2, X-Y2),
    can_move_over(Piece, X-Y2-Target2, Out2), Out2 \= pushMove,
    !.

can_move_over(Piece, _-emptySlot, normalMove) :- piece(Piece), !.
can_move_over(Piece, _-circleScr, normalMove) :- circle(Piece), !.
can_move_over(Piece, _-squareScr, normalMove) :- square(Piece), !.
    
can_move_over(Piece, X-Y-River, riverMove) :- 
    piece(Piece), 
    horizontal(River), 
    free_adjacent(horizontal, X-Y-Piece).

can_move_over(Piece, X-Y-River, riverMove) :-
    piece(Piece),
    vertical(River),
    free_adjacent(vertical, X-Y-Piece).

can_move_over(River, X-Y-Piece, pushMove) :-
    horizontal(River),
    piece(Piece),
    free_adjacent(horizontal, X-Y-Piece).

can_move_over(River, X-Y-Piece, pushMove) :-
    vertical(River),
    piece(Piece),
    free_adjacent(vertical, X-Y-Piece).

generate_orthogonal(X-Y, X1-Y1) :-  X1 is X, Y1 is Y + 1.
generate_orthogonal(X-Y, X1-Y1) :-  X1 is X, Y1 is Y - 1.
generate_orthogonal(X-Y, X1-Y1) :-  X1 is X + 1, Y1 is Y.
generate_orthogonal(X-Y, X1-Y1) :-  X1 is X - 1, Y1 is Y.

orthogonal_move(X, Y, X-Y-Moving/X1-Y1-Target/Outcome) :-
    generate_orthogonal(X-Y, X1-Y1),
    piece_in(Moving, X-Y),
    piece_in(Target, X1-Y1),
    can_move_over(Moving, X1-Y1-Target, Outcome).

get_right_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X2-Y1-Target/normalMove | Moves ]) :-
    X2 is X1 + 1,
    piece_in(Target, X2-Y1),
    can_move_over(Moving, X2-Y1-Target, normalMove),
    get_right_moves(X-Y-Moving, X2-Y1, Moves),
    !.
get_right_moves(X-Y-Moving, X1-Y1, Moves) :-
    X is X1 + 1,
    Y is Y1,
    get_right_moves(X-Y-Moving, X-Y1, Moves),
    !.
get_right_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X2-Y1-Target/riverMove]) :-
    X2 is X1 + 1,
    piece_in(Target, X2-Y1),
    can_move_over(Moving, X2-Y1-Target, riverMove),
    !.
get_right_moves(_, _, []).

get_left_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X2-Y1-Target/normalMove | Moves ]) :-
    X2 is X1 - 1,
    piece_in(Target, X2-Y1),
    can_move_over(Moving, X2-Y1-Target, normalMove),
    get_left_moves(X-Y-Moving, X2-Y1, Moves),
    !.
get_left_moves(X-Y-Moving, X1-Y1, Moves) :-
    X is X1 - 1,
    Y is Y1,
    get_left_moves(X-Y-Moving, X-Y1, Moves),
    !.
get_left_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X2-Y1-Target/riverMove]) :-
    X2 is X1 - 1,
    piece_in(Target, X2-Y1),
    can_move_over(Moving, X2-Y1-Target, riverMove),
    !.
get_left_moves(_, _, []).

get_up_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X1-Y2-Target/normalMove | Moves]) :-
    Y2 is Y1 - 1,
    piece_in(Target, X1-Y2),
    can_move_over(Moving, X1-Y2-Target, normalMove),
    get_up_moves(X-Y-Moving, X1-Y2, Moves),
    !.
get_up_moves(X-Y-Moving, X1-Y1, Moves) :-
    Y is Y1 - 1,
    X is X1,
    get_up_moves(X-Y-Moving, X1-Y, Moves),
    !.
get_up_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X1-Y2-Target/riverMove]) :-
    Y2 is Y1 - 1,
    piece_in(Target, X1-Y2),
    can_move_over(Moving, X1-Y2-Target, riverMove),
    !.
get_up_moves(_, _, []).

get_down_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X1-Y2-Target/normalMove | Moves ]) :-
    Y2 is Y1 + 1,
    piece_in(Target, X1-Y2),
    can_move_over(Moving, X1-Y2-Target, normalMove),
    get_down_moves(X-Y-Moving, X1-Y2, Moves),
    !.
get_down_moves(X-Y-Moving, X1-Y1, Moves) :-
    Y is Y1 + 1,
    X is X1,
    get_down_moves(X-Y-Moving, X1-Y, Moves),
    !.
get_down_moves(X-Y-Moving, X1-Y1, [X-Y-Moving/X1-Y2-Target/riverMove]) :-
    Y2 is Y1 + 1,
    piece_in(Target, X1-Y2),
    can_move_over(Moving, X1-Y2-Target, riverMove),
    !.
get_down_moves(_, _, []).

expand_move(X-Y-Piece/X1-Y1-River/riverMove, Moves) :-
    horizontal(River),
    !,
    get_right_moves(X-Y-Piece, X1-Y1, Moves1),
    get_left_moves(X-Y-Piece, X1-Y1, Moves2),
    append(Moves1, Moves2, Moves).

expand_move(X-Y-Piece/X1-Y1-River/riverMove, Moves) :- 
    vertical(River),
    !,
    get_up_moves(X-Y-Piece, X1-Y1, Moves1),
    get_down_moves(X-Y-Piece, X1-Y1, Moves2),
    append(Moves1, Moves2, Moves).

expand_move(X-Y-River/X1-Y1-Piece/pushMove, Moves) :-
    horizontal(River),
    !,
    get_right_moves(X-Y-Piece, X1-Y1, Moves1),
    get_left_moves(X-Y-Piece, X1-Y1, Moves2),
    append(Moves1, Moves2, Moves).

expand_move(X-Y-River/X1-Y1-Piece/pushMove, Moves) :-
    vertical(River),
    !,
    get_up_moves(X-Y-Piece, X1-Y1, Moves1),
    get_down_moves(X-Y-Piece, X1-Y1, Moves2),
    append(Moves1, Moves2, Moves).

generate_moves(X, Y, Moves) :-
    findall(Move, orthogonal_move(X, Y, Move), Moves).


