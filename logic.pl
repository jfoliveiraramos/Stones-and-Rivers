:- use_module(library(between)).

piece_in(Board, Piece, X/Y) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece).

slot_in(Slot, X/Y) :-
    get_empty_board(EmptyBoard),
    nth0(Y, EmptyBoard, Row),
    nth0(X, Row, Slot).

validate_piece([X-Y, Board, Turn]) :-
    piece_in(Board, Piece, X/Y),
    belongs_to(Piece, Turn).

replace_piece(Board, X/Y, NewPiece, NewBoard) :-
    nth0(Y, Board, Row, RestBoard),
    nth0(X, Row, _, RestRow), 
    nth0(X, NewRow, NewPiece, RestRow),
    nth0(Y, NewBoard, NewRow, RestBoard).

remove_piece(Board, X/Y, NewBoard) :-
    get_empty_board(EmptyBoard),
    nth0(Y, EmptyBoard, EmptyRow),
    nth0(X, EmptyRow, EmptyPlace), 
    replace_piece(Board, X/Y, EmptyPlace, NewBoard).


% -----------------------------------------------------

flip(squareStn, horizontal, squareHrz) :- !.
flip(squareStn, vertical, squareVrt) :- !.
flip(circleStn, horizontal, circleHrz) :- !.
flip(circleStn, vertical, circleVrt) :- !.
flip(squareHrz, squareStn) :- !.
flip(squareVrt, squareStn) :- !.
flip(circleHrz, circleStn) :- !.
flip(circleVrt, circleStn) :- !.

rotate(squareVrt, squareHrz) :- !.
rotate(squareHrz, squareVrt) :- !.
rotate(circleVrt, circleHrz) :- !.
rotate(circleHrz, circleVrt) :- !.

% -----------------------------------------------------

free_slot(Board, SlotPos) :-
    piece_in(Board, Piece, SlotPos),
    \+ piece(Piece).

can_move_over(Piece, Pos) :-
    slot_in(Slot, Pos),
    can_move_over_slot(Piece, Slot).

can_move(Board, Piece, Pos, normalMove) :- piece(Piece), piece_in(Board, emptySlot, Pos), !.
can_move(Board, Piece, Pos, normalMove) :- circle(Piece), piece_in(Board, circleScr, Pos), !.
can_move(Board, Piece, Pos, normalMove) :- square(Piece), piece_in(Board, squareScr, Pos), !.
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

generate_orthogonal(X/Y, X1/Y1) :-  X1 is X, Y1 is Y + 1.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X, Y1 is Y - 1.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X + 1, Y1 is Y.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X - 1, Y1 is Y.

orthogonal_move(Board, Pos1, Pos2-Outcome) :-
    generate_orthogonal(Pos1, Pos2),
    piece_in(Board, Moving, Pos1),
    can_move(Board, Moving, Pos2, Outcome).

right(X/Y, X1/Y) :- X1 is X + 1.
left(X/Y, X1/Y) :- X1 is X - 1.
up(X/Y, X/Y1) :- Y1 is Y - 1.
down(X/Y, X/Y1) :- Y1 is Y + 1.

get_direction_moves(Board, Piece, OgPos, Pos, Direction, [Move | Moves]) :-
    call(Direction, Pos, NewPos),
    can_move(Board, Piece, NewPos, normalMove),
    Move = OgPos-NewPos-normalMove,
    get_direction_moves(Board, Piece, OgPos, NewPos, Direction, Moves),
    !.
get_direction_moves(Board, Piece, OgPos, Pos, Direction, [Move]) :-
    call(Direction, Pos, NewPos),
    can_move(Board, Piece, NewPos, riverMove),
    Move = OgPos-NewPos-riverMove,
    !.
get_direction_moves(_, _, _, _, _, []).

get_right_moves(Board, Piece, OgPos, Start, Moves) :- get_direction_moves(Board, Piece, OgPos, Start, right, Moves).
get_left_moves(Board, Piece, OgPos, Start, Moves) :- get_direction_moves(Board, Piece, OgPos, Start, left, Moves).
get_up_moves(Board, Piece, OgPos, Start, Moves) :- get_direction_moves(Board, Piece, OgPos, Start, up, Moves).
get_down_moves(Board, Piece, OgPos, Start, Moves) :- get_direction_moves(Board, Piece, OgPos, Start, down, Moves).

expand_move(_, Positions-normalMove, _, _, Positions-normalMove) :- !.
expand_move(Board, MainMove, Piece, _, MainMove-Moves):-
    MainMove = Pos1-Pos2-riverMove,
    piece_in(Board, River, Pos2),
    horizontal(River),
    !,
    get_right_moves(Board, Piece, Pos1, Pos2, RightMoves),
    get_left_moves(Board, Piece, Pos1, Pos2, LeftMoves),
    append(LeftMoves, RightMoves, Moves).
    
expand_move(Board, MainMove, Piece, _, MainMove-Moves):-
    MainMove = Pos-Pos2-riverMove,
    piece_in(Board, River, Pos2),
    vertical(River),
    !,
    get_up_moves(Board, Piece, Pos, Pos2, RightMoves),
    get_down_moves(Board, Piece, Pos, Pos2, LeftMoves),
    append(LeftMoves, RightMoves, Moves).

expand_move(Board, MainMove, Pushed, PushingRiver, MainMove-Moves) :-
    MainMove = Pos-Pos2-pushMove,
    horizontal(PushingRiver),
    !,
    get_right_moves(Board, Pushed, Pos, Pos2, Moves1),
    get_left_moves(Board, Pushed, Pos, Pos2, Moves2),
    append(Moves1, Moves2, Moves).

expand_move(Board, MainMove, Pushed, PushingRiver, MainMove-Moves) :-
    MainMove = Pos-Pos2-pushMove,
    vertical(PushingRiver),
    !,
    get_up_moves(Board, Pushed, Pos, Pos2, Moves1),
    get_down_moves(Board, Pushed, Pos, Pos2, Moves2),
    append(Moves1, Moves2, Moves).

initial_moves(Board, Pos, Moves) :-
     findall(Move, orthogonal_move(Board, Pos, Move), Moves).

find_limit(Board, Piece, Direction, Pos, Limit) :- 
    call(Direction, Pos, NewPos),
    can_move(Board, Piece, NewPos, Outcome),
    (Outcome = normalMove ; Outcome = riverMove),
    !,
    find_limit(Board, Piece, Direction, NewPos, Limit).

find_limit(_, _, _, Limit, Limit).

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

get_move_in_direction(Board, Piece, Direction, RiverPos, Limit, NewPos-Outcome) :-
    between_pos(RiverPos, Limit, Direction, NewPos),
    can_move(Board, Piece, NewPos, Outcome),
    (Outcome = normalMove ; Outcome = riverMove).

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

develop(Board, Piece, Pos-_, NewMove) :-
    !,
    piece_in(Board, River, Pos),
    get_river_movement(Board, Piece, River, Pos, NewMove).

river_verifier(RiverPos-riverMove, VisitedRivers, UpdatedRivers) :-
    \+ memberchk(RiverPos, VisitedRivers),
    !,
    UpdatedRivers = [RiverPos | VisitedRivers].

river_verifier(_-normalMove, VisitedRivers, VisitedRivers) :- !.

full_develop(_, _, Pos-normalMove, _, [Pos-normalMove]) :- !.

full_develop(Board, Piece, Move, VisitedRivers, [Move | Moves]) :-
    develop(Board, Piece, Move, NewMove),
    river_verifier(NewMove, VisitedRivers, UpdatedRivers),
    full_develop(Board, Piece, NewMove, UpdatedRivers, Moves).


setup_develop(Board, Pos, NewPos-pushMove, Piece, TempBoard) :-
    piece_in(Board, Piece, NewPos),
    piece_in(Board, PushingRiver, Pos),
    remove_piece(Board, Pos, TempBoard1),
    replace_piece(TempBoard1, NewPos, PushingRiver, TempBoard),
    !.
setup_develop(Board, Pos, _, Piece, TempBoard) :- 
    piece_in(Board, Piece, Pos),
    remove_piece(Board, Pos, TempBoard),
    !.

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

valid_move(Board, Player, Move):-
    piece_in(Board, Piece, Pos),
    belongs_to(Piece, Player),
    valid_piece_move(Board, Pos, Move).
    
valid_moves(match-(Player-Board), Player, Moves) :-
    findall(Move, valid_move(Board, Player, Move), Moves).