:- dynamic board/1.

board([
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, squareScr, squareScr, squareScr, squareScr, squareScr, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, circleVrt, emptySlot, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, squareHrz, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, circleScr, circleScr, circleScr, circleScr, circleScr, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot]
]).

board_size(Board, Width, Height) :-
    Board = [Row | _],
    length(Board, Height),
    length(Row, Width).