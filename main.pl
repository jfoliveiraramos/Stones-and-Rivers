:- ensure_loaded('menu.pl').

default_settings(13/14-hum/hum).

play :-
    default_settings(DefautSettings),
    menu_loop(DefautSettings). 