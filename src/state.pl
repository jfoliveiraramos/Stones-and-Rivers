:- ensure_loaded('match.pl').
:- ensure_loaded('menu.pl').
:- ensure_loaded('gameover.pl').

% display_game(+GameState)
% Displays the current game state
display_game(GameState) :-
    GameState = menu,
    clear_screen,
    display_title,
    settings(Settings),
    display_settings(Settings),
    display_menu_options.
display_game(GameState) :-
    GameState = match-(Player-Board),
    clear_screen,
    display_board(Board),
    display_turn(Player),
    nl.
display_game(GameState) :-
    GameState = gameOver-Winner,
    clear_screen,
    display_title,
    display_winner(Winner),
    display_gameOver_options.
display_game(quit) :- clear_screen.

% proceed(+GameState, -NewGameState)
% Proceeds to the next game state.
proceed(GameState, NewGameState) :-
    GameState = menu,
    handle_menu(NewGameState).
proceed(GameState, NewGameState) :-
    GameState = match-_,
    handle_turn(GameState, PlayedGameState),
    verify_end_of_game(PlayedGameState, NewGameState).
proceed(GameState, NewGameState) :-
    GameState = gameOver-_,
    read_input(Input, validate_option, [1-2]), nl,
    gameOver_option(Input, NewGameState).
proceed(quit, end).
