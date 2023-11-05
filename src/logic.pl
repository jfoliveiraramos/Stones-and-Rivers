:- use_module(library(between)).

% board_size(+Board, -Width, -Height)
% Returns the width and height of the given board.
board_size(Board, Width, Height) :-
    Board = [Row | _],
    length(Board, Height),
    length(Row, Width).

% piece_in(+Board, ?Piece, ?X/Y)
% Finds a piece for a position in the board.
piece_in(Board, Piece, X/Y) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece).

% slot_in(?Slot, ?X/Y)
% Finds a slot for a position in the board.
slot_in(Slot, X/Y) :-
    get_empty_board(EmptyBoard),
    nth0(Y, EmptyBoard, Row),
    nth0(X, Row, Slot).

% validate_piece(+ArgumentsList)
% Validates if a provided position corresponds to a piece that belongs to the player.
validate_piece([X-Y, Board, Turn]) :-
    piece_in(Board, Piece, X/Y),
    belongs_to(Piece, Turn).

% place_piece(+Board, +X/Y, +NewPiece, -NewBoard)
% Places piece in a board for the provided position.
place_piece(Board, X/Y, NewPiece, NewBoard) :-
    nth0(Y, Board, Row, RestBoard),
    nth0(X, Row, _, RestRow), 
    nth0(X, NewRow, NewPiece, RestRow),
    nth0(Y, NewBoard, NewRow, RestBoard).

% remove_piece(+Board, +X/Y, -NewBoard)
% Removes a piece from a position in the board, preserving the respective slot.
remove_piece(Board, X/Y, NewBoard) :-
    get_empty_board(EmptyBoard),
    nth0(Y, EmptyBoard, EmptyRow),
    nth0(X, EmptyRow, EmptyPlace), 
    place_piece(Board, X/Y, EmptyPlace, NewBoard).

% flip(+Stone, +Direction, -River)
% Flips a stone to a river with the specified direction.
flip(squareStn, horizontal, squareHrz) :- !.
flip(squareStn, vertical, squareVrt) :- !.
flip(circleStn, horizontal, circleHrz) :- !.
flip(circleStn, vertical, circleVrt) :- !.

% flip(+River, -Stone)
% Flips a river to a stone.
flip(squareHrz, squareStn) :- !.
flip(squareVrt, squareStn) :- !.
flip(circleHrz, circleStn) :- !.
flip(circleVrt, circleStn) :- !.

% rotate(+River, -RotatedRiver)  
% Rotates a river.
rotate(squareVrt, squareHrz) :- !.
rotate(squareHrz, squareVrt) :- !.
rotate(circleVrt, circleHrz) :- !.
rotate(circleHrz, circleVrt) :- !.

% free_slot(+Board, ?SlotPos)
% Finds a free slot in the board.
free_slot(Board, SlotPos) :-
    piece_in(Board, Piece, SlotPos),
    \+ piece(Piece).

% can_move_over(?Piece, ?Pos)
% Checks if a piece can move over a position.
can_move_over(Piece, Pos) :-
    slot_in(Slot, Pos),
    can_move_over_slot(Piece, Slot).

% can_move(+Board, +Piece, +Pos, -Outcome)
% Checks if a piece can move to a position and returns the outcome of the move if it can.
can_move(Board, Piece, Pos, normalMove) :- 
    piece(Piece), 
    piece_in(Board, Slot, Pos),
    can_move_over_slot(Piece, Slot),
    !.
can_move(Board, Piece, Pos, riverMove) :- 
    piece(Piece), 
    can_move_over(Piece, Pos),
    piece_in(Board, River, Pos),
    horizontal(River).
can_move(Board, Piece, Pos, riverMove) :-
    piece(Piece),
    can_move_over(Piece, Pos),
    piece_in(Board, River, Pos),
    vertical(River).
can_move(Board, River, Pos, pushMove) :-
    horizontal(River),
    can_move_over(River, Pos),
    piece_in(Board, Piece, Pos),
    piece(Piece).
can_move(Board, River, Pos, pushMove) :-
    vertical(River),
    can_move_over(River, Pos),
    piece_in(Board, Piece, Pos),
    piece(Piece).

% generate_orthogonal(+Pos, -NewPos)
% Generates a position that is orthogonal to the provided position.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X, Y1 is Y + 1.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X, Y1 is Y - 1.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X + 1, Y1 is Y.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X - 1, Y1 is Y.

% orthogonal_move(+Board, +Pos, -Move)
% Generates a move that is orthogonal to the provided position for the piece in the provided position.
orthogonal_move(Board, Pos1, Pos2-Outcome) :-
    generate_orthogonal(Pos1, Pos2),
    piece_in(Board, Moving, Pos1),
    can_move(Board, Moving, Pos2, Outcome).

% right(Pos, NewPos)
% Generates a position that is to the right of the provided position.
right(X/Y, X1/Y) :- X1 is X + 1.

% left(Pos, NewPos)
% Generates a position that is to the left of the provided position.
left(X/Y, X1/Y) :- X1 is X - 1.

% up(Pos, NewPos)
% Generates a position that is above the provided position.
up(X/Y, X/Y1) :- Y1 is Y - 1.

% down(Pos, NewPos)
% Generates a position that is below the provided position.
down(X/Y, X/Y1) :- Y1 is Y + 1.

% find_limit(+Board, +Piece, +Direction, +Pos, -Limit)
% Finds the limit for a river momevement of the provided piece in the given direction.
find_limit(Board, Piece, Direction, Pos, Limit) :- 
    call(Direction, Pos, NewPos),
    can_move(Board, Piece, NewPos, Outcome),
    (Outcome = normalMove ; Outcome = riverMove),
    !,
    find_limit(Board, Piece, Direction, NewPos, Limit).
find_limit(_, _, _, Limit, Limit).

% between_pos(+RiverPos, +Limit, +Direction, -NewPos)
% Finds a position between the river position and the limit in the given direction.
between_pos(RiverX/Y, Left/Y, left, X/Y) :-
    Right is RiverX - 1,
    between(Left, Right, X).
between_pos(RiverX/Y, Right/Y, right, X/Y) :-
    Left is RiverX + 1,
    between(Left, Right, X).
between_pos(X/RiverY, X/Up, up, X/Y) :-
    Down is RiverY - 1,
    between(Up, Down, Y).
between_pos(X/RiverY, X/Down, down, X/Y) :-
    Up is RiverY + 1,
    between(Up, Down, Y).

% get_move_in_direction(+Board, +Piece, +Direction, +RiverPos, +Limit, -NewMove)
% Finds a move, that is subsequent to a river movement or a push, in the given direction for the provided piece.
get_move_in_direction(Board, Piece, Direction, RiverPos, Limit, NewPos-Outcome) :-
    between_pos(RiverPos, Limit, Direction, NewPos),
    can_move(Board, Piece, NewPos, Outcome),
    (Outcome = normalMove ; Outcome = riverMove).

% get_river_movement(+Board, +Piece, +River, +RiverPos, -Move)
% Finds a river movement for the provided piece in the given river.
get_river_movement(Board, Piece, River, RiverPos, Move) :-
    horizontal(River),
    find_limit(Board, Piece, right, RiverPos, Limit),
    get_move_in_direction(Board, Piece, right, RiverPos, Limit, Move).
get_river_movement(Board, Piece, River, RiverPos, Move) :-
    horizontal(River),
    find_limit(Board, Piece, left, RiverPos, Limit),
    get_move_in_direction(Board, Piece, left, RiverPos, Limit, Move).
get_river_movement(Board, Piece, River, RiverPos, Move) :-
    vertical(River),
    find_limit(Board, Piece, up, RiverPos, Limit),
    get_move_in_direction(Board, Piece, up, RiverPos, Limit, Move).
get_river_movement(Board, Piece, River, RiverPos, Move) :-
    vertical(River),
    find_limit(Board, Piece, down, RiverPos, Limit),
    get_move_in_direction(Board, Piece, down, RiverPos, Limit, Move).

% develop(+Board, +Piece, +Move, -NewMove)
% Develops a move into the immediate next ramifications if it can.
develop(Board, Piece, Pos-_, NewMove) :-
    !,
    piece_in(Board, River, Pos),
    get_river_movement(Board, Piece, River, Pos, NewMove).

% river_verifier(+RiverMove, +VisitedRivers, -UpdatedRivers)
% Verifies if a river of the given river move has already been visited.
% If it has not, it adds it to the visited rivers list. 
river_verifier(RiverPos-riverMove, VisitedRivers, UpdatedRivers) :-
    \+ memberchk(RiverPos, VisitedRivers),
    !,
    UpdatedRivers = [RiverPos | VisitedRivers].
river_verifier(_-normalMove, VisitedRivers, VisitedRivers) :- !.

% full_develop(+Board, +Piece, +Move, +VisitedRivers, -Moves)
% Develops a move into all of its possible ramifications.
full_develop(_, _, Pos-normalMove, _, [Pos-normalMove]) :- !.
full_develop(Board, Piece, Move, VisitedRivers, [Move | Moves]) :-
    develop(Board, Piece, Move, NewMove),
    river_verifier(NewMove, VisitedRivers, UpdatedRivers),
    full_develop(Board, Piece, NewMove, UpdatedRivers, Moves).

% setup_develop(+Board, +Pos, +Move, +Piece, -TempBoard)
% Creates a temporary board with the provided move necessary setup applied to the provided position.
setup_develop(Board, Pos, NewPos-pushMove, Piece, TempBoard) :-
    piece_in(Board, Piece, NewPos),
    piece_in(Board, PushingRiver, Pos),
    remove_piece(Board, Pos, TempBoard1),
    place_piece(TempBoard1, NewPos, PushingRiver, TempBoard),
    !.
setup_develop(Board, Pos, _, Piece, TempBoard) :- 
    piece_in(Board, Piece, Pos),
    remove_piece(Board, Pos, TempBoard),
    !.

% valid_piece_move(+Board, +Pos, -Move)
% Finds a valid move for the piece in the provided position.
valid_piece_move(Board, Pos, Pos-(move-MoveSet)) :-
    orthogonal_move(Board, Pos, Move),
    setup_develop(Board, Pos, Move, Piece, TempBoard),  
    Move = RiverPos-_,
    full_develop(TempBoard, Piece, Move, [RiverPos], MoveSet).
valid_piece_move(Board, Pos, Pos-(flip-vertical)) :-
    piece_in(Board, Piece, Pos),
    stone(Piece).
valid_piece_move(Board, Pos, Pos-(flip-horizontal)) :-
    piece_in(Board, Piece, Pos),
    stone(Piece).
valid_piece_move(Board, Pos, Pos-(flip-river)) :-
    piece_in(Board, Piece, Pos),
    river(Piece).
valid_piece_move(Board, Pos, Pos-rotate) :-
    piece_in(Board, Piece, Pos),
    river(Piece).

% valid_move(+Board, +Player, -Move)
% Finds a valid move for the provided player.
valid_move(Board, Player, Move):-
    piece_in(Board, Piece, Pos),
    belongs_to(Piece, Player),
    valid_piece_move(Board, Pos, Move).
    
% valid_moves(+Match, +Player, -Moves)
% Finds all valid moves for the provided player in the provided match.
valid_moves(match-(Player-Board), Player, Moves) :-
    findall(Move, valid_move(Board, Player, Move), Moves).