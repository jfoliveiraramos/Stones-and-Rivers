% stone(?Piece)
% Defines the pieces that are stones.
stone(squareStn) :- !.
stone(circleStn) :- !.

% river(?Piece)
% Defines the pieces that are rivers.
river(squareVrt) :- !.
river(circleVrt) :- !.
river(squareHrz) :- !.
river(circleHrz) :- !.

% horizontal(?Piece)
% Defines the pieces that are horizontal rivers.
horizontal(squareHrz) :- !.
horizontal(circleHrz) :- !.

% vertical(?Piece)
% Defines the pieces that are vertical rivers.
vertical(squareVrt) :- !.
vertical(circleVrt) :- !.

% circle(?Piece)
% Defines the pieces that are circles.
circle(circleStn).
circle(circleVrt).
circle(circleHrz).

% square(?Piece)
% Defines the pieces that are squares.
square(squareStn).
square(squareVrt).
square(squareHrz).

% piece(?Piece)
% Checks if the given piece is a valid piece.
piece(Piece) :- stone(Piece) ; river(Piece).

% can_move_over(?Piece, ?Slot)
% Checks if the given piece can move over the given score slot.
can_score(Piece, circleScr) :- circle(Piece), !.
can_score(Piece, squareScr) :- square(Piece), !.

% can_move_over(?Piece, ?Slot)
% Checks if the given piece can move over the given slot.
can_move_over_slot(Piece, emptySlot) :- piece(Piece), !.
can_move_over_slot(Piece, Slot) :- can_score(Piece, Slot), !.

% belongs_to(?Piece, ?Player)
% Checks if the given piece belongs to the given player.
belongs_to(Piece, player_b) :- circle(Piece).
belongs_to(Piece, player_a) :- square(Piece).

% is_scorer_of(?Player, ?Slot)
% Checks if the given player is the scorer of the given score slot.
is_scorer_of(player_b, circleScr).
is_scorer_of(player_a, squareScr).