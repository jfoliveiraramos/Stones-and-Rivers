:- dynamic emptyBoard/1.

get_empty_board(Board) :- emptyBoard(Board).

set_emptyBoard(Board) :-
    retractall(emptyBoard(_)),
    assert(emptyBoard(Board)).

:- dynamic player_level/2.

player_level(player_a, human).
player_level(player_b, human).

is_human(Player) :- player_level(Player, human).
is_bot(Player) :- \+ is_human(Player).
is_easy(Player) :- player_level(Player, level1).
is_hard(Player) :- player_level(Player, level2).

get_players(PlayerA/PlayerB) :-
    player_level(player_a, PlayerA),
    player_level(player_b, PlayerB).

set_player(Player, Type) :-
    retractall(player_level(Player, _)),
    assert(player_level(Player, Type)).

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

