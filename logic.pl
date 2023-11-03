:- use_module(library(between)).

piece_in(Board, Piece, X/Y) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece).

validate_piece([X-Y, Board, Turn]) :-
    piece_in(Board, Piece, X/Y),
    belongs_to(Piece, Turn).

get_piece(Board, Turn, Piece, X/Y) :-
    write('Provide the coordinates of the piece you wish to play, as follows: X-Y\n'),
    read_input(X-Y, validate_piece, [Board, Turn], 'piece'),
    piece_in(Board, Piece, X/Y),
    !.

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
right(X/Y, X1/Y) :- X1 is X + 1.
left(X/Y, X1/Y) :- X1 is X - 1.
up(X/Y, X/Y1) :- Y1 is Y - 1.
down(X/Y, X/Y1) :- Y1 is Y + 1.

free_adjacent(Board, horizontal, Piece, Pos):-
    right(Pos, Right),
    can_move(Board, Piece, Right, Outcome), 
    Outcome \= pushMove,
    !.
free_adjacent(Board, horizontal, Piece, Pos):-
    left(Pos, Left),
    can_move(Board, Piece, Left, Outcome), 
    Outcome \= pushMove,
    !.
    
free_adjacent(Board, vertical, Piece, Pos):-
    up(Pos, Up),
    can_move(Board, Piece, Up, Outcome), 
    Outcome \= pushMove,
    !.
free_adjacent(Board, vertical, Piece, Pos):-
    down(Pos, Down),
    can_move(Board, Piece, Down, Outcome), 
    Outcome \= pushMove,
    !.

can_move(Board, Piece, Pos, normalMove) :- piece(Piece), piece_in(Board, emptySlot, Pos), !.
can_move(Board, Piece, Pos, normalMove) :- circle(Piece), piece_in(Board, circleScr, Pos), !.
can_move(Board, Piece, Pos, normalMove) :- square(Piece), piece_in(Board, squareScr, Pos), !.
can_move(Board, Piece, Pos, riverMove) :- 
    piece(Piece), 
    piece_in(Board, River, Pos),
    horizontal(River), 
    free_adjacent(Board, horizontal, Piece, Pos).
can_move(Board, Piece, Pos, riverMove) :-
    piece(Piece),
    piece_in(Board, River, Pos),
    vertical(River),
    free_adjacent(Board, vertical, Piece, Pos).
can_move(Board, River, Pos, pushMove) :-
    horizontal(River),
    piece_in(Board, Piece, Pos),
    piece(Piece),
    free_adjacent(Board, horizontal, Piece, Pos).
can_move(Board, River, Pos, pushMove) :-
    vertical(River),
    piece_in(Board, Piece, Pos),
    piece(Piece),
    free_adjacent(Board, vertical, Piece, Pos).

generate_orthogonal(X/Y, X1/Y1) :-  X1 is X, Y1 is Y + 1.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X, Y1 is Y - 1.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X + 1, Y1 is Y.
generate_orthogonal(X/Y, X1/Y1) :-  X1 is X - 1, Y1 is Y.

orthogonal_move(Board, Pos1, Pos1-Pos2-Outcome) :-
    generate_orthogonal(Pos1, Pos2),
    piece_in(Board, Moving, Pos1),
    can_move(Board, Moving, Pos2, Outcome).

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

setup_expand(FinalBoard, _-normalMove, _, _, FinalBoard) :- !.

setup_expand(Board, Pos1-_-riverMove, Piece, _, FinalBoard) :-
    piece_in(Board, Piece, Pos1),
    replace_piece(Board, Pos1, emptySlot, FinalBoard),
    !.

setup_expand(Board, Pos1-Pos2-pushMove, Piece, PushingRiver, FinalBoard) :-
    piece_in(Board, PushingRiver, Pos1),
    piece_in(Board, Piece, Pos2),
    replace_piece(Board, Pos1, emptySlot, TempBoard),
    replace_piece(TempBoard, Pos2, PushingRiver, FinalBoard).

% expand_moves(_, [], _, []) :- !.
% expand_moves(Board, [Move | Moves], Piece, [ExpandedMove | ExpandedMoves]) :-
%     expand_move(Board, Move, Piece, ExpandedMove),
%     expand_moves(Board, Moves, Piece, ExpandedMoves).

full_expand_move(Board, Move, Piece, PushingRiver, Move-ExpandedMoves) :-
    expand_move(Board, Move, Piece, PushingRiver, Move-Moves),
    full_expand_moves(Board, Moves, Piece, PushingRiver, ExpandedMoves).

full_expand_moves(_, [], _, []) :- !.
full_expand_moves(Board, [Move | Moves], Piece, PushingRiver, [ExpandedMove | ExpandedMoves]) :-
    full_expand_move(Board, Move, Piece, PushingRiver, ExpandedMove),
    full_expand_moves(Board, Moves, Piece, PushingRiver, ExpandedMoves).

expand_first_moves(_, [], []) :- !.
expand_first_moves(Board, [Move | Moves], [ExpandedMove | ExpandedMoves]) :-
    setup_expand(Board, Move, Piece, PushingRiver, TempBoard),
    full_expand_move(TempBoard, Move, Piece, PushingRiver, ExpandedMove),
    expand_first_moves(Board, Moves, ExpandedMoves).

generate_moves(Board, Pos, Moves) :-
    findall(Move, orthogonal_move(Board, Pos, Move), Moves).

valid_moves(Board, Pos, Moves) :-
    findall(Move, orthogonal_move(Board, Pos, Move), InitialMoves),
    expand_first_moves(Board, InitialMoves, Moves).