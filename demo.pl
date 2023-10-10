
/*

Game where you need to travel from Start to End

Start -> Mid -> End

Click d to advance a position
Click a to go back a position

*/

/* 
getState(start, mid, Input) :-
    Input is 100, % ASCCI code for d
    !,
    write('Advanced to MID'), nl, nl.

getState(mid, end, Input) :-
    Input is 100, % ASCCI code for d
    !,
    write('Advanced to END'), nl, nl.

getState(mid, start, Input) :-
    Input is 97, % ASCCI code for a
    !,
    write('Backed to START'), nl, nl.

getState(end, mid, Input) :-
    Input is 97, % ASCCI code for a
    !,
    write('Backed to MID'), nl, nl.

getState(S, S, _) :- 
    write('--- bad input ---'), nl.

gameLoop(end) :-
    !,
    write('Game Over'), nl, nl.

gameLoop(OldState) :-
    !,
    write('Input: '), 
    get_code(Input),
    skip_line,
    getState(OldState, NewState, Input),
    gameLoop(NewState).

start :- gameLoop(start).

*/

:- use_module(library(random)).
:- consult('data.pl').  

switch_turn(square, circle).
switch_turn(circle, square).

get_input(Input) :-
    get_code(C), skip_line,
    Input is C - 48.

gameLoop(T, piece_move, river) :-
    write('| Pick a move\n'), 
    write('1. Move\n'),
    write('2. Flip\n'),
    write('3. Rotate\n'),
    write('Enter: '),
    get_input(M),
    write('Move: '), write(M), nl,
    switch_turn(T, T2),
    gameLoop(T2, piece_pick).

gameLoop(T, piece_move, stone) :-
    write('| Pick a move\n'), 
    write('1. Move\n'),
    write('2. Flip\n'),
    write('Enter: '),
    get_input(M),
    write('Move: '), write(M), nl,
    switch_turn(T, T2),
    gameLoop(T2, piece_pick).

gameLoop(T, piece_pick) :-
    write('Turn: '), write(T), nl,
    write('| Pick a piece\n'), 
    write('Enter X: '),
    get_input(X),
    write('Enter Y: '),
    get_input(Y),
    piece(T, X, Y, S),
    write('Piece state: '), write(S), nl,
    gameLoop(T, piece_move, S).    

start(T) :- 
    write('Game Started'), nl, nl,
    gameLoop(T, piece_pick).

random_turn(0, square).
random_turn(1, circle).

random_turn(T) :-
    random(0,2,P),
    random_turn(P, T).

play :-
    random_turn(Turn),
    write(Turn), write(' starts'), nl,
    start(Turn).