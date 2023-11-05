% is_score(+Board, +Slot, -Pos)
% Returns the position of a score slot if it is occupied.
is_score(Board, Slot, Pos) :-
    slot_in(Slot, Pos),
    piece_in(Board, Piece, Pos),
    can_score(Piece, Slot).

% all_slots_occupied(+Board, +Slot)
% Checks if all the score slots corresponding to a given slot are occupied.
all_slots_occupied(Board, Slot) :-
    findall(Pos, is_score(Board, Slot, Pos), OccupiedSlots),
    findall(Pos, slot_in(Slot, Pos), Slots),
    length(OccupiedSlots, Length),
    length(Slots, Length).

% game_over(+GameState, -Winner)
% Checks if the game is over and returns the winner.
game_over(match-(_-Board), Winner) :- 
    is_scorer_of(Winner, Slot),
    all_slots_occupied(Board, Slot).
    
% verify_end_of_game(+GameState, -NewGameState)
% Verifies if the game is over and returns the new game state.
% If the game is over, the game over menu is shown, otherwise the game continues.
verify_end_of_game(GameState, gameOver-Winner) :- game_over(GameState, Winner), !.
verify_end_of_game(GameState, GameState).

% gameOver_option(+Option, -GameState)
% Handles the game over menu and returns the new game state for the provided option.
gameOver_option(1, GameState) :- 
    get_size(Size),
    initial_state(Size, GameState), !.
gameOver_option(2, menu) :- !.