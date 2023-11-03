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

can_score(Piece, circleScr) :- circle(Piece), !.
can_score(Piece, squareScr) :- square(Piece), !.

can_move_over_slot(Piece, emptySlot) :- piece(Piece), !.
can_move_over_slot(Piece, Slot) :- can_score(Piece, Slot), !.

% belongs_to(_, _) :- !.
belongs_to(Piece, player_b) :- circle(Piece), !.
belongs_to(Piece, player_a) :- square(Piece), !.

is_scorer_of(player_b, circleScr) :- !.
is_scorer_of(player_a, squareScr) :- !.

board_size(Board, Width, Height) :-
    Board = [Row | _],
    length(Board, Height),
    length(Row, Width).