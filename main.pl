:- ensure_loaded('create.pl').
:- ensure_loaded('draw.pl').
:- ensure_loaded('game.pl').
:- ensure_loaded('logic.pl').
:- ensure_loaded('menu.pl').
:- ensure_loaded('rules.pl').
:- ensure_loaded('sprites.pl').


default_settings(13/14-hum/hum).

play :-
    default_settings(DefautSettings),
    menu_loop(DefautSettings). 