:- ensure_loaded('create.pl').
:- ensure_loaded('display.pl').
:- ensure_loaded('gameover.pl').
:- ensure_loaded('match.pl').
:- ensure_loaded('logic.pl').
:- ensure_loaded('menu.pl').
:- ensure_loaded('rules.pl').
:- ensure_loaded('settings.pl').
:- ensure_loaded('sprites.pl').
:- ensure_loaded('state.pl').
:- ensure_loaded('utils.pl').

play :- game_loop(menu). 

game_loop(end) :- !.

game_loop(GameState) :-
    display_game(GameState),
    proceed(GameState, NewGameState),
    game_loop(NewGameState).