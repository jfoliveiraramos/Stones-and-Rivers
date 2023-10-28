% # Data

piece_to_text(empty, ' ') :- !.
piece_to_text(square, 'A') :- !.
piece_to_text(circle, 'B') :- !.
piece_to_text(square_score, 'b') :- !.
piece_to_text(circle_score, 'a') :- !.  

board(B) :-
    B = [
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, square_score, square_score, square_score, square_score, square_score, empty, empty, empty, empty],
        [empty, empty, empty, circle, circle, circle, circle, circle, circle, circle, empty, empty, empty],
        [empty, empty, empty, circle, circle, circle, circle, circle, circle, circle, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, square, square, square, square, square, square, square, empty, empty, empty],
        [empty, empty, empty, square, square, square, square, square, square, square, empty, empty, empty],
        [empty, empty, empty, empty, circle_score, circle_score, circle_score, circle_score, circle_score, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty]
    ].

board_size(Board, Width, Height) :-
    Board = [Row | _],
    length(Board, Height),
    length(Row, Width).

% # Draw

clear_screen:- 
    write('\33\[2J').

% ## Draw Axis

draw_x(N, N) :- !.

draw_x(Width, N) :-
    N // 10 =:= 0, !,
    format('    ~d    ', [N]),
    N1 is N + 1,
    draw_x(Width, N1).

draw_x(Width, N) :-
    format('   ~d    ', [N]),
    N1 is N + 1,
    draw_x(Width, N1).

draw_top_x_axis(Width) :-
    write('y/x  '),
    draw_x(Width, 0),
    nl,nl.

draw_bottom_x_axis(Width) :-
    write('     '),
    draw_x(Width, 0),
    nl,nl.

draw_vertical_connector(0) :- !.

draw_vertical_connector(N) :-
    write('    |    '),
    N1 is N - 1,
    draw_vertical_connector(N1).

draw_vertical_connection_line(N) :-
    write('     '),
    draw_vertical_connector(N),
    nl.

% ## Draw Rows

draw_piece_middle([]) :- !.

draw_piece_middle([Piece | Pieces]) :-
    piece_to_text(Piece, T),
    format('-   ~s   -', [T]),
    draw_piece_middle(Pieces).

draw_row(Row, Y) :-
    Y // 10 =:= 0, !,
    length(Row, Width),
    draw_vertical_connection_line(Width),
    nl,
    format('~d   -', [Y]), draw_piece_middle(Row),format('-   ~d', [Y]),
    nl,
    nl.

draw_row(Row, Y) :-
    length(Row, Width),
    draw_vertical_connection_line(Width),
    nl,
    format('~d  -', [Y]), draw_piece_middle(Row),format('-  ~d', [Y]),
    nl,
    nl.

draw_rows([], H, H) :- !.

draw_rows([Row | Rows], H, Y) :-
    draw_row(Row, Y),
    Y1 is Y + 1,
    draw_rows(Rows, H, Y1).

% ## Draw Board

draw(Board) :-
    clear_screen,
    board_size(Board, W, H),
    draw_top_x_axis(W),
    draw_rows(Board, H, 0),
    draw_bottom_x_axis(W), 
    nl.

% # Play

play :- 
    board(B),
    draw(B).



    
