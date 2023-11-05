:- use_module(library(random)).

% get_turn(?Turn, ?Player)
% Returns the player for the randomly generated turn value. 
get_turn(0, player_a).
get_turn(1, player_b).

% decide_first_turn(-Player)
% Returns a randomly decided player to start the game.
decide_first_turn(Player) :-
    random(0, 2, R),
    get_turn(R, Player).

% create_row(+Width, -Row)
% Creates a row of empty slots with the given width.
create_row(Width, Row) :-
    findall(emptySlot, between(1, Width, _), Row).

% create_plain_board(+Width, +Height, -Board)
% Creates a board with the given width and height, filled with empty slots.
create_plain_board(Width, Height, Board) :-
    create_row(Width, Row),
    findall(Row, between(1, Height, _), Board).

% set_scores(+Board, +Width, +Height, +PlayerPieces, -FinalBoard)
% Sets up the score spaces in the board.
set_scores(Board, Width, Height, PlayerPieces, FinalBoard) :-
    X is (Width // 2 - PlayerPieces // 4) + 1,
    SquareScoreY is (Height - 8) // 2,
    N is PlayerPieces // 2 - 2,
    put_pieces(Board, squareScr, X, SquareScoreY, N, Board1),
    CircleScoreY is SquareScoreY + 7, 
    put_pieces(Board1, circleScr, X, CircleScoreY, N, FinalBoard).

% put_piece(+Board, +X, +Y, +Piece, -NewBoard)
% Sets up the provided piece in a horizontal line in the board.
% The provided piece is placed in the provided coordinates (X, Y) and the subsequent N-1 spaces.
put_pieces(Board, _, _, _, 0, Board) :- !.
put_pieces(Board, Piece, X, Y, N, FinalBoard) :-
    place_piece(Board, X/Y, Piece, NewBoard),
    N1 is N - 1,
    X1 is X + 1,
    put_pieces(NewBoard, Piece, X1, Y, N1, FinalBoard).

% populate(+Board, +Width, +Height, +PlayerPieces, -FinalBoard)
% Sets up the pieces in the board. The amount of pieces for each player is provided by PlayerPieces.
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

%For Test Purposes
% create_board(_, _,[
%     [emptySlot, circleScr, circleScr, circleScr],
%     [emptySlot, emptySlot, squareStn, emptySlot],
%     [emptySlot, emptySlot, emptySlot, squareStn],
%     [squareScr, circleStn, circleStn, circleStn]
% ]) :- 
%     set_emptyBoard([
%         [emptySlot, circleScr, circleScr, circleScr],
%         [emptySlot, emptySlot, emptySlot, emptySlot],
%         [emptySlot, emptySlot, emptySlot, emptySlot],
%         [squareScr, emptySlot, emptySlot, emptySlot]
%     ]), !.   

% create_board(+Widht, +Height, -Board)
% Creates a board with the given width and height.
create_board(Width, Height, Board) :-
    create_plain_board(Width, Height, PlainBoard),
    PlayerPieces is (Width // 2 + 1) * 2,
    set_scores(PlainBoard, Width, Height, PlayerPieces, EmptyBoard),
    set_emptyBoard(EmptyBoard),
    povoate(EmptyBoard, Width, Height, PlayerPieces, Board).

% initial_state(+Size, -GameState)
% Creates the initial game state for a match.
initial_state(Width/Height, match-MatchState) :- 
    MatchState=Player-Board,
    create_board(Width, Height, Board),
    decide_first_turn(Player).

