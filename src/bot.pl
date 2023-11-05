:- use_module(library(random)).
:- use_module(library(lists)).
:- ensure_loaded('logic.pl').
:- ensure_loaded('match.pl').

% piece_distance_from(+Board, +Player, +Pos, -Distance)
% Returns the distance between a piece of the provided player and the provided position.
piece_distance_from(Board, Player, X1/Y1, Distance) :-
    belongs_to(Piece, Player),
    piece_in(Board, Piece, X2/Y2),
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

% slot_distance(+Board, +Player, -TotalDistance)
% Returns the total distance between all pieces of the provided player and a free slot.
slot_distance(Board, Player, TotalDistance) :-
    is_scorer_of(Player, Slot),
    slot_in(Slot, Pos),
    free_slot(Board, Pos),
    findall(Distance, piece_distance_from(Board, Player, Pos, Distance), Distances),
    sumlist(Distances, TotalDistance).

% value(+GameState, +Player, -Value)
% Returns the value of the provided game state for the provided player, which corresponds 
% to the sum of the distances between all pieces of the player and a free slot.
value(GameState, Player, Value) :-
    GameState = match-(_-Board),
    findall(Distance, slot_distance(Board, Player, Distance), Distances),
    sumlist(Distances, Value).

% simulate_move(+GameState, +Player, +Moves, -MoveValue)
% Returns the value of the gamestate, consequen of a move chosen from the provided list of moves.
simulate_move(GameState, Player, Moves, Move-Value) :-
    member(Move, Moves),  
    move(GameState, Move, NewGameState),
    value(NewGameState, Player, Value).

% better_move(+MoveValue1, +MoveValue2, -ChosenMoveValue)
% Returns the move with the better value. Lower is better.
better_move(Move-Value, _-Value1, Move-Value) :- Value < Value1, !.
better_move(_-Value, Move1-Value1, Move1-Value1) :- Value > Value1, !.
better_move(MoveValue, MoveValue1, ChosenMoveValue) :- 
    random(0, 2, Random), 
    nth0(Random, [MoveValue, MoveValue1], ChosenMoveValue), 
    !.

% best_move(+MovesValues, -ChosenMoveValue)
% Returns the move with the best value, in the provied list. Lower is better.   
best_move([Move-Value], Move-Value) :- !.
best_move([MoveValue | MovesValues], ChosenMoveValue) :-
    best_move(MovesValues, MoveValue1),
    better_move(MoveValue, MoveValue1, ChosenMoveValue),
    !.

% choose_move(+GameState, +Player, +Level, -Move)
% Returns a move for the provided player, in the provided game state, according to the provided level.
% Level 1: Chooses a move randomly.
% Level 2: Chooses the move with the best value. Lower is better.
choose_move(GameState, Player, level1, Move) :-
    valid_moves(GameState, Player, Moves),
    length(Moves, Length),
    random(0, Length, Index),
    nth0(Index, Moves, Move).
choose_move(GameState, Player, level2, ChosenMove) :-
    valid_moves(GameState, Player, Moves),
    findall(Move-Value, simulate_move(GameState, Player, Moves, Move-Value), MovesValues),
    best_move(MovesValues, ChosenMove-_).
    