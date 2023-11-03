
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

replace_piece(Board, X/Y, NewPiece, NewBoard) :-
    nth0(Y, Board, Row, RestBoard),
    nth0(X, Row, _, RestRow), 
    nth0(X, NewRow, NewPiece, RestRow),
    nth0(Y, NewBoard, NewRow, RestBoard).

outcome_text(normalMove, '').
outcome_text(riverMove, ' (River Movement)').
outcome_text(pushMove, ' (River Push)').

print_move(_/_-X/Y-normalMove, N) :-
    outcome_text(normalMove, OutcomeText),
    format('~d. ~d,~d~s\n', [N, X, Y, OutcomeText]),
    !.
print_move(_/_-X/Y-Outcome, N) :-
    outcome_text(Outcome, OutcomeText),
    format('~d. ~d,~d~s\n', [N, X, Y, OutcomeText]).

print_moves(Moves) :- 
    print_moves(Moves, 1), 
    nl.
print_moves([], _) :- !.
print_moves([Move | Moves], N) :-
    print_move(Move, N),
    N1 is N + 1,
    print_moves(Moves, N1).

get_direction(1, horizontal) :- !.
get_direction(2, vertical) :- !.   

execute_followup(Board, Piece, _-Pos2-normalMove, NewBoard) :-
    !,
    replace_piece(Board, Pos2, Piece, NewBoard).

execute_followup(Board, Piece, _-_-_-Moves, NewBoard) :-
    print_moves(Moves),
    length(Moves, Length),
    read_input(Input, validate_option, [1-Length]),
    nth1(Input, Moves, Move),
    expand_move(Board, Move, Piece, _, Expanded),
    execute_followup(Board, Piece, Expanded, NewBoard).

execute_move(Board, Pos1-Pos2-normalMove, NewBoard) :-
    !,
    piece_in(Board, Piece, Pos1),
    replace_piece(Board, Pos1, emptySlot, Board1),
    replace_piece(Board1, Pos2, Piece, NewBoard).

execute_move(Board, Move, NewBoard) :-
    Move = Pos1-_-riverMove,
    !,
    piece_in(Board, Piece, Pos1),
    replace_piece(Board, Pos1, emptySlot, Board1),
    expand_move(Board1, Move, Piece, _, Expanded),
    write('\nRiver Movement! \n'),
    execute_followup(Board1, Piece, Expanded, NewBoard).

execute_move(Board, Move, NewBoard) :-
    Move = Pos1-Pos2-pushMove,
    !,
    piece_in(Board, River, Pos1),
    piece_in(Board, Piece, Pos2),
    replace_piece(Board, Pos1, emptySlot, Board1),
    replace_piece(Board1, Pos2, River, Board2),
    execute_play(Board2, flip, Pos2, Board3),
    expand_move(Board3, Move, Piece, River, Expanded),
    write('\nRiver Push! \n'),
    execute_followup(Board3, Piece, Expanded, NewBoard).

execute_play(Board, move, Pos, NewBoard) :-
    generate_moves(Board, Pos, Moves),
    write('\nAvailable Moves for :\n'),
    print_moves(Moves),
    length(Moves, Length),
    read_input(Input, validate_option, [1-Length]),
    nth1(Input, Moves, Move),
    execute_move(Board, Move, NewBoard).

execute_play(Board, flip, Pos, NewBoard) :-
    piece_in(Board, Piece, Pos),
    (Piece = squareStn; Piece = circleStn),
    !,
    write('1. Horizontal\n'),
    write('2. Vertical\n'),
    write('Provide the direction of the flip\n'),
    read_input(Option, validate_option, [1-2]),
    get_direction(Option, Direction),
    flip(Piece, Direction, Flipped),
    replace_piece(Board, Pos, Flipped, NewBoard).

execute_play(Board, flip, Pos, NewBoard) :-
    !,
    piece_in(Board, Piece, Pos),
    flip(Piece, Flipped),
    replace_piece(Board, Pos, Flipped, NewBoard).

execute_play(Board, rotate, Pos, NewBoard) :-
    !,
    piece_in(Board, Piece, Pos),
    rotate(Piece, Rotated),
    replace_piece(Board, Pos, Rotated, NewBoard).

get_move(Board, Turn, Move-Pos) :-
    get_piece(Board, Turn, Piece, Pos),
    select_play(Piece, Move).

move(Turn-Board-Players, Move-Pos, NewTurn-NewBoard-Players) :-
    execute_play(Board, Move, Pos, NewBoard),
    switch_turn(Turn, NewTurn).

play_turn(Turn-Board-Players, NewGameState) :-
    get_move(Board, Turn, Move),
    move(Turn-Board-Players, Move, NewGameState).

game_loop(GameState) :-
    display_game(GameState),
    play_turn(GameState, NewGameState),
    game_loop(NewGameState).
    
