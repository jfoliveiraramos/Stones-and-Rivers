:- ensure_loaded('create.pl').

validate_size([Width-Height]) :-
    number(Width), number(Height),
    Width >= 5, Height >= 10,
    Width mod 2 =:= 1,
    Height mod 2 =:= 0.

validate_players([A-B]) :-
    memberchk(A, [hum, pc1, pc2]),
    memberchk(B, [hum, pc1, pc2]).

start(NewGameState) :-
    get_size(Size),
    initial_state(Size, NewGameState).

change_players(menu) :-
    write('Provide the new players in the following format A-B, where A is player A and B is player B\n\n'),
    write('| hum - Human\n'),
    write('| pc1 - Computer (Easy)\n'),
    write('| pc2 - Computer (Hard)\n\n'),
    write('Example: hum-pc2\n\n'),
    read_input(A-B, validate_players, [], 'players'),
    set_players(A/B).

change_board_size(menu) :-
    write('Provide the new board width and height in the following format width-height.\n'),
    write('| width is odd && width >= 5.\n'),
    write('| height is even && height >= 10.\n\n'),
    write('Example: 13-14\n\n'),
    read_input(Width-Height, validate_size, [], 'width and height'),
    set_size(Width/Height).

menu_option(1, NewGameState) :- !, start(NewGameState).
menu_option(2, NewGameState) :- !, change_players(NewGameState).
menu_option(3, NewGameState) :- !, change_board_size(NewGameState).
menu_option(0, quit).

