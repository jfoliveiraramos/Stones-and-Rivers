
/*

Game where you need to travel from Start to End

Start -> Mid -> End

Click d to advance a position
Click a to go back a position

*/

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