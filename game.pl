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

board_size(Width, Height) :-
    board(B),
    B = [Row | T],
    length(B, Height),
    length(Row, Width).





    
