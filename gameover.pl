is_score(Board, Slot, Pos) :-
    slot_in(Slot, Pos),
    piece_in(Board, Piece, Pos),
    can_score(Piece, Slot).

all_slots_occupied(Board, Slot) :-
    findall(Pos, is_score(Board, Slot, Pos), OccupiedSlots),
    findall(Pos, slot_in(Slot, Pos), Slots),
    length(OccupiedSlots, Length),
    length(Slots, Length).

game_over(match-(_-Board), Winner) :- 
    is_scorer_of(Winner, Slot),
    all_slots_occupied(Board, Slot).
    
verify_end_of_game(GameState, gameOver-Winner) :- game_over(GameState, Winner), !.
verify_end_of_game(GameState, GameState).

gameOver_option(1, GameState) :- 
    get_size(Size),
    initial_state(Size, GameState), !.
gameOver_option(2, menu) :- !.