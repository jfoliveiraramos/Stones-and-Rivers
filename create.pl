create_row(Width, Row) :-
    findall(emptySlot, between(1, Width, _), Row).

create_empty_board(Width, Height, Board) :-
    create_row(Width, Row),
    findall(Row, between(1, Height, _), Board).

put_pieces(Board, _, _, _, 0, Board) :- !.

put_pieces(Board, Piece, X, Y, N, FinalBoard) :-
    replace_piece(Board, X, Y, Piece, NewBoard),
    N1 is N - 1,
    X1 is X + 1,
    put_pieces(NewBoard, Piece, X1, Y, N1, FinalBoard).

povoate(Board, Width, Height, PlayerPieces, circleStn, FinalBoard) :-
    X is (Width // 2 - PlayerPieces // 4),
    SquareScoreY is (Height - 8) // 2,
    CircleY1 is SquareScoreY + 1,
    CircleY2 is CircleY1 + 1,
    N is PlayerPieces // 2,
    ScoreN is N - 2,
    ScoreX is X + 1,
    put_pieces(Board, squareScr, ScoreX, SquareScoreY, ScoreN, Board1),
    put_pieces(Board1, circleStn, X, CircleY1, N, Board2),
    put_pieces(Board2, circleStn, X, CircleY2, N, Board3),
    SquareY1 is CircleY2 + 3,
    SquareY2 is SquareY1 + 1,
    CircleScoreY is SquareY2 + 1, 
    put_pieces(Board3, squareStn, X, SquareY1, N, Board4),
    put_pieces(Board4, squareStn, X, SquareY2, N, Board5),
    put_pieces(Board5, circleScr, ScoreX, CircleScoreY, ScoreN, FinalBoard).

create_board(Width, Height, Board) :-
    create_empty_board(Width, Height, EmptyBoard),
    PlayerPieces is (Width // 2 + 1) * 2,
    povoate(EmptyBoard, Width, Height, PlayerPieces, circleStn, Board).

initial_state(Width/Height, Players, Turn-Board-Players) :- 
    create_board(Width, Height, Board),
    decide_first_turn(Turn).

