:- ensure_loaded('create.pl').

validate_size([Width-Height]) :-
    number(Width), number(Height),
    Width >= 5, Height >= 10,
    Width mod 2 =:= 1,
    Height mod 2 =:= 0.

validate_players([A-B]) :-
    memberchk(A, [human, level1, level2]),
    memberchk(B, [human, level1, level2]).

start(NewGameState) :-
    get_size(Size),
    initial_state(Size, NewGameState).

change_players(menu) :-
    write('Provide the new players in the following format A-B, where A is player A and B is player B\n\n'),
    write('| human - Human\n'),
    write('| level1 - Easy Bot\n'),
    write('| level2 - Computer (Hard)\n'),
    write('Example: human-level2\n\n'),
    write('You may type 0 to return to the menu.\n\n'),
    read_input(Input, validate_players, [], 'players'),
    Input = A-B,
    set_players(A/B),
    !.
change_players(menu).

change_board_size(menu) :-
    write('Provide the new board width and height in the following format width-height.\n'),
    write('| width is odd && width >= 5.\n'),
    write('| height is even && height >= 10.\n'),
    write('Example: 13-14\n\n'),
    write('You may type 0 to return to the menu.\n\n'),
    read_input(Input, validate_size, [], 'width and height'),
    Input = Width-Height,
    set_size(Width/Height),
    !.
change_board_size(menu).

handle_menu(NewGameState) :-
    raw_input(Input, validate_option, [1-3]), nl,
    menu_option(Input, NewGameState), 
    !.

menu_option(1, NewGameState) :- !, start(NewGameState).
menu_option(2, NewGameState) :- !, change_players(NewGameState).
menu_option(3, NewGameState) :- !, change_board_size(NewGameState).
menu_option(0, quit) :- !.
