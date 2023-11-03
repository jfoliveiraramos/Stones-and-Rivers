get_turn(player_a, 0).
get_turn(player_b, 1).

decide_first_turn(T) :-
    random(0, 2, R),
    get_turn(T, R).

create_row(Width, Row) :-
    findall(emptySlot, between(1, Width, _), Row).

create_plain_board(Width, Height, Board) :-
    create_row(Width, Row),
    findall(Row, between(1, Height, _), Board).

put_pieces(Board, _, _, _, 0, Board) :- !.

put_pieces(Board, Piece, X, Y, N, FinalBoard) :-
    replace_piece(Board, X/Y, Piece, NewBoard),
    N1 is N - 1,
    X1 is X + 1,
    put_pieces(NewBoard, Piece, X1, Y, N1, FinalBoard).

set_scores(Board, Width, Height, PlayerPieces, FinalBoard) :-
    X is (Width // 2 - PlayerPieces // 4) + 1,
    SquareScoreY is (Height - 8) // 2,
    N is PlayerPieces // 2 - 2,
    put_pieces(Board, squareScr, X, SquareScoreY, N, Board1),
    CircleScoreY is SquareScoreY + 7, 
    put_pieces(Board1, circleScr, X, CircleScoreY, N, FinalBoard).

povoate(Board, Width, Height, PlayerPieces, FinalBoard) :-
    X is (Width // 2 - PlayerPieces // 4),
    CircleY1 is (Height - 8) // 2 + 1,
    CircleY2 is CircleY1 + 1,
    N is PlayerPieces // 2,
    put_pieces(Board, circleStn, X, CircleY1, N, Board1),
    put_pieces(Board1, circleStn, X, CircleY2, N, Board2),
    SquareY1 is CircleY2 + 3,
    SquareY2 is SquareY1 + 1,
    put_pieces(Board2, squareStn, X, SquareY1, N, Board3),
    put_pieces(Board3, squareStn, X, SquareY2, N, FinalBoard).


% For Test Purposes
create_board(_, _,[
    [circleHrz, emptySlot, emptySlot, circleVrt],
    [squareScr, emptySlot, emptySlot, emptySlot],
    [squareScr, circleScr, circleStn, circleStn],
    [squareStn, emptySlot, circleHrz, circleHrz]
]) :- 
    set_emptyBoard([
        [emptySlot, emptySlot, emptySlot, emptySlot],
        [squareScr, emptySlot, emptySlot, emptySlot],
        [squareScr, circleScr, emptySlot, emptySlot],
        [emptySlot, emptySlot, emptySlot, emptySlot]
    ]), !.   

create_board(Width, Height, Board) :-
    create_plain_board(Width, Height, PlainBoard),
    PlayerPieces is (Width // 2 + 1) * 2,
    set_scores(PlainBoard, Width, Height, PlayerPieces, EmptyBoard),
    set_emptyBoard(EmptyBoard),
    povoate(EmptyBoard, Width, Height, PlayerPieces, Board).

initial_state(Width/Height, match-MatchState) :- 
    MatchState=Turn-Board,
    create_board(Width, Height, Board),
    decide_first_turn(Turn).

