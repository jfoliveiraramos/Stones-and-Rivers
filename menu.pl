:- ensure_loaded('create.pl').
:- ensure_loaded('game.pl').

validate_option([Option, L-U]) :- between(L, U, Option).

validade_size([Width-Height]) :-
    Width >= 5, Height >= 10,
    Width mod 2 =:= 1,
    Height mod 2 =:= 0.

validate_players([A-B]) :-
    memberchk(A, [hum, pc1, pc2]),
    memberchk(B, [hum, pc1, pc2]).

read_input(Input, Validator, Arguments) :-
    read_input(Input, Validator, Arguments, 'option').

read_input(Input, Validator, Arguments, _) :-
    read(Input), 
    call(Validator, [Input | Arguments]),
    !.

read_input(Input, Validator, Arguments, OptionType) :-
    repeat,
    format('Invalid! Provide valid ~s.\n', [OptionType]),
    read(Input),
    call(Validator, [Input | Arguments]).


start(Size-Players) :- 
    initial_state(Size, Players, GameState),
    game_loop(GameState).

player(hum, 'Human') :- !.
player(pc1, 'Computer (Easy)') :- !.
player(pc2, 'Computer (Hard)') :- !.

change_players(Size-_, Size-A/B) :-
    write('Provide the new players in the following format A-B, where A is player A and B is player B\n\n'),
    write('| hum - Human\n'),
    write('| pc1 - Computer (Easy)\n'),
    write('| pc2 - Computer (Hard)\n\n'),
    write('Example: hum-pc2\n\n'),
    read_input(A-B, validate_players, [], 'players').

change_board_size(_-Players, Width/Height-Players) :-
    write('Provide the new board width and height in the following format width-height.\n'),
    write('| width is odd && width >= 5.\n'),
    write('| height is even && height >= 10.\n\n'),
    write('Example: 13-14\n\n'),
    read_input(Width-Height, validade_size, [], 'width and height').

menu_option(1, Settings, Settings) :- !, start(Settings).
menu_option(2, Settings, NewSettings) :- !, change_players(Settings, NewSettings).
menu_option(3, Settings, NewSettings) :- !, change_board_size(Settings, NewSettings).

display_settings(Width/Height-A/B) :-
    write('Current settings:\n\n'),
    format('Board size: ~d by ~d\n', [Width, Height]),
    player(A, PlayerA),
    player(B, PlayerB),
    format('Player A: ~w\n', [PlayerA]),
    format('Player B: ~w\n\n', [PlayerB]).

menu_loop(Settings) :-
    clear_screen,
    draw_title,
    display_settings(Settings),
    write('1. Start game\n'),
    write('2. Change players\n'),
    write('3. Change board size\n'),
    read_input(Input, validate_option, [1-3]), nl,
    menu_option(Input, Settings, NewSettings),
    menu_loop(NewSettings).
