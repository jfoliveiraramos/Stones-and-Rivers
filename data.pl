stone(squareStn) :- !.
stone(circleStn) :- !.
river(squareVrt) :- !.
river(circleVrt) :- !.
river(squareHrz) :- !.
river(circleHrz) :- !.

horizontal(squareHrz) :- !.
horizontal(circleHrz) :- !.

vertical(squareVrt) :- !.
vertical(circleVrt) :- !.

circle(circleStn) :- !.
circle(circleVrt) :- !.
circle(circleHrz) :- !.

square(squareStn) :- !.
square(squareVrt) :- !.
square(squareHrz) :- !.

piece(Piece) :- stone(Piece) ; river(Piece).

belongs_to(Piece, player_a) :- square(Piece), !.
belongs_to(Piece, player_b) :- circle(Piece), !.

:- dynamic board/1.

board([
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, squareScr, squareScr, squareScr, squareScr, squareScr, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, circleStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, squareHrz, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, squareVrt, squareStn, squareStn, squareStn, squareStn, squareStn, squareStn, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, circleScr, circleScr, circleScr, circleScr, circleScr, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot],
    [emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot, emptySlot]
]) :- !.

backup_board :-
    board(B),
    retractall(backup_board(_)),
    asserta((backup_board(B) :- !)).

restore_board :-
    backup_board(B),
    retractall(board(_)),
    asserta(board(B)).

board_size(Board, Width, Height) :-
    Board = [Row | _],
    length(Board, Height),
    length(Row, Width).