:- ensure_loaded('match.pl').
:- ensure_loaded('menu.pl').
:- ensure_loaded('gameover.pl').

display_game(GameState) :-
    GameState = menu,
    clear_screen,
    draw_title,
    settings(Settings),
    display_settings(Settings),
    display_menu_options.

display_game(GameState) :-
    GameState = match-(Player-Board),
    clear_screen,
    draw_board(Board),
    get_players(Players),
    draw_turn(Player, Players),
    nl.

display_game(GameState) :-
    GameState = gameOver-Winner,
    clear_screen,
    draw_title,
    draw_winner(Winner),
    display_gameOver_options.

display_game(quit) :- clear_screen.

proceed(GameState, NewGameState) :-
    GameState = menu,
    raw_input(Input, validate_option, [1-3]), nl,
    menu_option(Input, NewGameState).

proceed(GameState, NewGameState) :-
    GameState = match-_,
    handle_turn(GameState, PlayedGameState),
    verify_end_of_game(PlayedGameState, NewGameState).

proceed(GameState, NewGameState) :-
    GameState = gameOver-_,
    read_input(Input, validate_option, [1-2]), nl,
    gameOver_option(Input, NewGameState).

proceed(quit, end).
