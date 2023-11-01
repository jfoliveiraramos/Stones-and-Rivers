:- ensure_loaded('draw.pl').
:- ensure_loaded('moves.pl').
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

replace_piece(X, Y, NewPiece, Board) :-
    nth0(Y, Board, Row, RestBoard),
    nth0(X, Row, _, RestRow), 
    nth0(X, NewRow, NewPiece, RestRow),
    nth0(Y, NewBoard, NewRow, RestBoard),
    retract((board(Board) :- !)),
    asserta((board(NewBoard) :- !)).

replace_piece(X, Y, NewPiece) :-
    board(Board),
    replace_piece(X, Y, NewPiece, Board).

outcome_text(normalMove, '').
outcome_text(riverMove, ' (River Movement)').
outcome_text(pushMove, ' (River Push)').

print_moves([], _) :- !.
print_moves([_-_-_/X1-Y1-_/Outcome | Moves], N) :-
    outcome_text(Outcome, OutcomeText),
    format('~d. ~d,~d~s\n', [N, X1, Y1, OutcomeText]),
    N1 is N + 1,
    print_moves(Moves, N1).

get_direction(1, horizontal) :- !.
get_direction(2, vertical) :- !.   

move(X-Y-Piece/X1-Y1-_/normalMove) :-
    !,
    replace_piece(X, Y, emptySlot),
    replace_piece(X1, Y1, Piece).

move(X-Y-Piece/X1-Y1-River/riverMove) :-
    write('River Movement!\n'),
    expand_move(X-Y-Piece/X1-Y1-River/riverMove, Moves),
    length(Moves, Length),
    print_moves(Moves, 1),
    read(Input),
    validate_option(Input, 1-Length),
    nth1(Input, Moves, Move),
    move(Move).

move(X-Y-River/X1-Y1-Piece/pushMove) :-
    write('River Push!\n'),
    piece_in(Piece, X1-Y1),
    replace_piece(X, Y, emptySlot),
    replace_piece(X1, Y1, River),
    expand_move(X-Y-River/X1-Y1-Piece/pushMove, Moves),
    length(Moves, Length),
    print_moves(Moves, 1),
    read(Input),
    validate_option(Input, 1-Length),
    nth1(Input, Moves, Move),
    move(Move).

execute_play(move, X, Y, _Piece) :-
    generate_moves(X, Y, Moves),
    print_moves(Moves, 1),
    read(Input),
    length(Moves, Length),
    validate_option(Input, 1-Length),
    nth1(Input, Moves, Move),
    move(Move).

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

play_turn(Turn, NewTurn) :-
    turn(Turn, _, TurnText),
    write(TurnText),
    get_piece(Turn, Piece, X, Y),
    select_play(Piece, Move),
    execute_play(Move, X, Y, Piece),
    swicth_turn(Turn, NewTurn).

game_loop(Turn) :-
    %repeat,
    board(B),
    draw(B),
    play_turn(Turn, NewTurn),
    game_loop(NewTurn).

play :- 
    decide_first_turn(T),
    game_loop(T).
    
