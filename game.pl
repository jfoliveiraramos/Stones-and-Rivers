
:- use_module(library(random)).
:- use_module(library(lists)).

% # Play

play_option(1, move).
play_option(2, flip).
play_option(3, rotate).

select_stone_play(Move) :-
    write('1. Move\n'),
    write('2. Flip\n'),
    write('Select a move for the stone\n'),
    read_input(Option, validate_option, [1-2]),
    play_option(Option, Move), 
    !.

select_river_play(Move) :-
    write('1. Move\n'),
    write('2. Flip\n'),
    write('3. Rotate\n'),
    write('Select a move for the river\n'),
    read_input(Option, validate_option, [1-3]),
    play_option(Option, Move), 
    !.

select_play(squareStn, Move) :- !, select_stone_play(Move).
select_play(circleStn, Move) :- !, select_stone_play(Move).

select_play(squareVrt, Move) :- !, select_river_play(Move).
select_play(squareHrz, Move) :- !, select_river_play(Move).
select_play(circleVrt, Move) :- !, select_river_play(Move).
select_play(circleHrz, Move) :- !, select_river_play(Move). 

switch_turn(player_a, player_b).
switch_turn(player_b, player_a).

replace_piece(Board, X, Y, NewPiece, NewBoard) :-
    nth0(Y, Board, Row, RestBoard),
    nth0(X, Row, _, RestRow), 
    nth0(X, NewRow, NewPiece, RestRow),
    nth0(Y, NewBoard, NewRow, RestBoard).

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

execute_play(Board, move, X, Y, NewBoard) :-
    write('Currently Not Moving\n'),
    read(_).
    % generate_moves(X, Y, Moves),
    % print_moves(Moves, 1),
    % read(Input),
    % length(Moves, Length),
    % validate_option(Input, 1-Length),
    % nth1(Input, Moves, Move),
    % move(Move).

execute_play(Board, flip, X, Y, NewBoard) :-
    piece_in(Board, Piece, X-Y),
    (Piece = squareStn; Piece = circleStn),
    !,
    write('1. Horizontal\n'),
    write('2. Vertical\n'),
    write('Provide the direction of the flip\n'),
    read_input(Option, validate_option, [1-2]),
    get_direction(Option, Direction),
    flip(Piece, Direction, Flipped),
    replace_piece(Board, X, Y, Flipped, NewBoard).

execute_play(Board, flip, X, Y, NewBoard) :-
    !,
    piece_in(Board, Piece, X-Y),
    flip(Piece, Flipped),
    replace_piece(Board, X, Y, Flipped, NewBoard).

execute_play(Board, rotate, X, Y, NewBoard) :-
    !,
    piece_in(Board, Piece, X-Y),
    rotate(Piece, Rotated),
    replace_piece(Board, X, Y, Rotated, NewBoard).

get_move(Board, Turn, Move-X/Y) :-
    get_piece(Board, Turn, X, Y, Piece),
    select_play(Piece, Move).

move(Turn-Board-Players, Move-X/Y, NewTurn-NewBoard-Players) :-
    execute_play(Board, Move, X, Y, NewBoard),
    switch_turn(Turn, NewTurn).

play_turn(Turn-Board-Players, NewGameState) :-
    get_move(Board, Turn, Move),
    move(Turn-Board-Players, Move, NewGameState).

game_loop(GameState) :-
    display_game(GameState),
    play_turn(GameState, NewGameState),
    game_loop(NewGameState).
    
