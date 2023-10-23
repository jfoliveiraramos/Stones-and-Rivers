board_size(13, 14).

board(B) :-
    B = [
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, square_score, square_score, square_score, square_score, square_score, empty, empty, empty, empty],
        [empty, empty, empty, circle, circle, circle, circle, circle, circle, circle, empty, empty, empty],
        [empty, empty, empty, circle, circle, circle, circle, circle, circle, circle, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, square, square, square, square, square, square, square, empty, empty, empty],
        [empty, empty, empty, square, square, square, square, square, square, square, empty, empty, empty],
        [empty, empty, empty, empty, circle_score, circle_score, circle_score, circle_score, circle_score, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty]
    ].

:-use_module(library(lists)).

find(X, Y, Spot) :-
    board(B),
    nth0(Y, B, Row),
    nth0(X, Row, Spot).

sprite(empty, ' ').
sprite(square, 'X').
sprite(circle, 'O').
sprite(square_score, 'x').
sprite(circle_score, 'o').

empty_line(0, _) :- !, nl.

empty_line(X, 0) :- !, nl.

empty_line(X, N) :-
    write(' | '),
    X1 is X - 1,
    empty_line(X1, N).

display_sprite(P, L, L) :-
    !,
    sprite(P, S),
    write(' '), write(S), write('-').

display_sprite(P, _, 1) :-
    !,
    sprite(P, S),
    write('-'), write(S), write(' ').

display_sprite(P, _, _) :-
    sprite(P, S),
    write('-'), write(S), write('-').

display_row([], _, 0):- nl.

display_row([P | T], L, C):-
    display_sprite(P, L, C),
    C1 is C - 1,
    display_row(T, L, C1).

display_board([]).

display_board([Row | T], N) :-
    N // 10 > 0,
    !,
    write('   '),
    length(Row, L),
    empty_line(L, N),
    write(N),
    N1 is N + 1,
    write(' '),
    display_row(Row, L, L),
    display_board(T, N1).

display_board([Row | T], N):-
    write('   '),
    length(Row, L),
    empty_line(L, N),
    write(N),
    N1 is N + 1,
    write('  '), 
    display_row(Row, L, L),
    display_board(T, N1).

display_gridX(X, X) :- !.

display_gridX(N, X):-
    N // 10 > 0,
    !,
    write(' '),
    write(N),
    N1 is N + 1,
    display_gridX(N1, X).

display_gridX(N, X):-
    write(' '),
    write(N),
    write(' '),
    N1 is N + 1,
    display_gridX(N1, X).

display_x(X) :-
    write('   '),
    display_gridX(0, X), nl.

paint :-
    board_size(X, _),
    display_x(X),
    board(B),
    display_board(B, 0).
