:- ensure_loaded('create.pl').
:- ensure_loaded('game.pl').

start(Size-Players) :- 
    initial_state(Size, Players, GameState),
    game_loop(GameState).

player(hum, 'Human') :- !.
player(pc1, 'Computer (Easy)') :- !.
player(pc2, 'Computer (Hard)') :- !.

change_players(Size-_, Size-A/B) :-
    write('Provide the new players in the following format A-B, where A is player A and B is player B\n\n'),
    write('Example: hum-pc2\n\n'),
    write('hum - Human\n'),
    write('pc1 - Computer (Easy)\n'),
    write('pc2 - Computer (Hard)\n'),
    read(A-B).

change_board_size(_-Players, Width/Height-Players) :-
    write('Provide the new board width and height in the following format Width-Height.\n'),
    write('Width must be odd and greater than 5.\n'),
    write('Height must be even and greater than 10.\n\n'),
    write('Example: 13-14\n\n'),
    read(Width-Height),
    Width >= 5, Height >= 10,
    Width mod 2 =:= 1,
    Height mod 2 =:= 0.

menu_option(1, Settings, Settings) :- !, start(Settings).
menu_option(2, Settings, NewSettings) :- !, change_players(Settings, NewSettings).
menu_option(3, Settings, NewSettings) :- !, change_board_size(Settings, NewSettings).
menu_option(4, Settings, Settings) :- !, clear_screen, draw_title.

display_settings(Width/Height-A/B) :-
    write('Current settings:\n\n'),
    format('Board size: ~d by ~d\n', [Width, Height]),
    player(A, PlayerA),
    player(B, PlayerB),
    format('Player A: ~w\n', [PlayerA]),
    format('Player B: ~w\n\n', [PlayerB]).

read_option(Input, Range) :-
    read(Input), 
    validate_option(Input, Range).

read_option(Input, Range) :-
    repeat,
    write('Invalid option! Provide a valid option.\n'),
    read(Input),
    validate_option(Input, Range).

menu_loop(Settings) :-
    clear_screen,
    draw_title,
    display_settings(Settings),
    write('1. Start game\n'),
    write('2. Change players\n'),
    write('3. Change board size\n'),
    write('4. Reset screen\n'),
    read_option(Input, 1-4), nl,
    menu_option(Input, Settings, NewSettings),
    menu_loop(NewSettings).
