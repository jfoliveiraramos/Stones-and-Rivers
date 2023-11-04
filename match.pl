
:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('logic.pl').
:- ensure_loaded('settings.pl').
:- ensure_loaded('utils.pl').

query_piece(Board, Player, Piece, X/Y) :-
    write('Provide the coordinates of the piece you wish to play, as follows: X-Y\n'),
    read_input(X-Y, validate_piece, [Board, Player], 'piece'),
    piece_in(Board, Piece, X/Y),
    !.

play_stone_option(1, move-player).
play_stone_option(2, flip-vertical).
play_stone_option(3, flip-horizontal).

play_river_option(1, move-player).
play_river_option(2, flip-river).
play_river_option(3, rotate).

select_stone_play(Move) :-
    nl,
    write('Select a move for the stone\n'),
    write('You may type 0 to cancel a move at any time.\n\n'),
    write('1. Move\n'),
    write('2. Flip to vertical river\n'),
    write('3. Flip to horizontal river\n\n'),
    read_input(Option, validate_option, [1-3]),
    play_stone_option(Option, Move), 
    !.

select_river_play(Move) :-
    nl,
    write('Select a move for the river\n'),
    write('You may type 0 to cancel a move at any time.\n\n'),
    write('1. Move\n'),
    write('2. Flip to stone\n'),
    write('3. Rotate 90 degrees\n\n'),
    read_input(Option, validate_option, [1-3]),
    play_river_option(Option, Move), 
    !.

select_play(squareStn, Move) :- !, select_stone_play(Move).
select_play(circleStn, Move) :- !, select_stone_play(Move).

select_play(squareVrt, Move) :- !, select_river_play(Move).
select_play(squareHrz, Move) :- !, select_river_play(Move).
select_play(circleVrt, Move) :- !, select_river_play(Move).
select_play(circleHrz, Move) :- !, select_river_play(Move). 

switch_turn(match-(player_a-Board), match-(player_b-Board)) :- !.
switch_turn(match-(player_b-Board), match-(player_a-Board)) :- !.

outcome_text(normalMove, '').
outcome_text(riverMove, ' (River Movement)').
outcome_text(pushMove, ' (River Push)').

print_move(X/Y-normalMove, N) :-
    outcome_text(normalMove, OutcomeText),
    format('~d. ~d,~d~s\n', [N, X, Y, OutcomeText]),
    !.
print_move(X/Y-Outcome, N) :-
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

execute_move(Board, Pos1, Pos2-normalMove, NewBoard) :-
    !,
    piece_in(Board, Piece, Pos1),
    remove_piece(Board, Pos1, Board1),
    replace_piece(Board1, Pos2, Piece, NewBoard).

execute_move(Board, Move, NewBoard) :-
    Move = Pos1-_-riverMove,
    !,
    piece_in(Board, Piece, Pos1),
    remove_piece(Board, Pos1, Board1),
    expand_move(Board1, Move, Piece, _, Expanded),
    write('\nRiver Movement!\n'),
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
    initial_moves(Board, Pos, Moves),
    write('\nChoose one of the available moves for the selected piece.\n\n'),
    print_moves(Moves),
    length(Moves, Length),
    read_input(Input, validate_option, [1-Length]),
    nth1(Input, Moves, Move),
    execute_move(Board, Pos, Move, NewBoard).

% write_moves([], _).
% write_moves([Move | Moves], N) :-
%     N1 is N + 1,
%     format('~d. ~w\n', [N, Move]),
%     write_moves(Moves, N1).

move(match-(Player-Board), Pos-rotate, match-(Player-NewBoard)) :-
    !,
    piece_in(Board, Piece, Pos),
    rotate(Piece, Rotated),
    replace_piece(Board, Pos, Rotated, NewBoard).

move(match-(Player-Board), Pos-(flip-river), match-(Player-NewBoard)) :-
    !,
    piece_in(Board, Piece, Pos),
    flip(Piece, Flipped),
    replace_piece(Board, Pos, Flipped, NewBoard).

move(match-(Player-Board), Pos-(flip-Direction), match-(Player-NewBoard)) :-
    !,
    piece_in(Board, Piece, Pos),
    flip(Piece, Direction, Flipped),
    replace_piece(Board, Pos, Flipped, NewBoard).

move(match-(Player-Board), Pos-(move-player), match-(Player-NewBoard)) :-
    !,
    execute_play(Board, move, Pos, NewBoard).

move(match-(Player-Board), Pos-move-[FinalPos-normalMove], match-(Player-NewBoard)) :- 
    !,
    piece_in(Board, Piece, Pos),
    remove_piece(Board, Pos, Board1),
    replace_piece(Board1, FinalPos, Piece, NewBoard).

move(match-(Player-Board), Pos-move-[_-riverMove | Moves], match-(Player-NewBoard)) :- 
    !,
    last(_, FinalPos-normalMove, Moves),
    piece_in(Board, Piece, Pos),
    remove_piece(Board, Pos, Board1),
    replace_piece(Board1, FinalPos, Piece, NewBoard).

move(match-(Player-Board), Pos-move-[PushedPos-pushMove | Moves], match-(Player-NewBoard)) :- 
    !,
    last(_, FinalPos-normalMove, Moves),
    piece_in(Board, Piece, PushedPos),
    piece_in(Board, River, Pos),
    flip(River, FlippedRiver),
    remove_piece(Board, Pos, Board1),
    replace_piece(Board1, PushedPos, FlippedRiver, Board2),
    replace_piece(Board2, FinalPos, Piece, NewBoard).

handle_turn(GameState, NewGameState) :-
    GameState = match-(Player-_),
    is_pc(Player),
    wait_for_input,
    valid_moves(GameState, Player, Moves),
    length(Moves, Length),
    random(0, Length, Index),
    nth0(Index, Moves, Move),
    move(GameState, Move, GameState1),
    switch_turn(GameState1, NewGameState),
    !.

handle_turn(GameState, NewGameState) :-
    GameState = match-(Player-Board),
    is_human(Player),
    query_move(Board, Player, Move),
    move(GameState, Move, GameState1),
    switch_turn(GameState1, NewGameState),
    !.
  
handle_turn(GameState, GameState).
