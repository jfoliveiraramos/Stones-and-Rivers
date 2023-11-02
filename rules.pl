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

board_size(Board, Width, Height) :-
    Board = [Row | _],
    length(Board, Height),
    length(Row, Width).