:- dynamic board/1.

belongs_to(squareStn, player_a).
belongs_to(squareVrt, player_a).
belongs_to(squareHrz, player_a).

belongs_to(circleStn, player_b).
belongs_to(circleVrt, player_b).
belongs_to(circleHrz, player_b).

board([
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, squareTrg, squareTrg, squareTrg, squareTrg, squareTrg, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, circleTrg, circleTrg, circleTrg, circleTrg, circleTrg, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot]
]) :- !.

board_size(Board, Width, Height) :-
    Board = [Row | _],
    length(Board, Height),
    length(Row, Width).