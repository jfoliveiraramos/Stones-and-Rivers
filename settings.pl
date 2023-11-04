:- dynamic emptyBoard/1.

get_empty_board(Board) :- emptyBoard(Board).

set_emptyBoard(Board) :-
    retractall(emptyBoard(_)),
    assert(emptyBoard(Board)).

:- dynamic player_type/2.

player_type(player_a, hum).
player_type(player_b, hum).

is_human(Player) :- player_type(Player, hum).
is_pc(Player) :- \+ is_human(Player).
is_easy_pc(Player) :- player_type(Player, pc1).
is_hard_pc(Player) :- player_type(Player, pc2).

get_players(PlayerA/PlayerB) :-
    player_type(player_a, PlayerA),
    player_type(player_b, PlayerB).

set_player(Player, Type) :-
    retractall(player_type(Player, _)),
    assert(player_type(Player, Type)).

set_players(PlayerA/PlayerB) :-
    set_player(player_a, PlayerA),
    set_player(player_b, PlayerB).

:- dynamic size/2.

size(13,14).

set_size(Width/Height) :-
    retractall(size(_,_)),
    assert(size(Width, Height)).

get_size(Width/Height) :- size(Width, Height).

settings(Size-Players) :-
    get_size(Size),
    get_players(Players).

