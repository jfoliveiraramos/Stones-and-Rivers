:- ensure_loaded('sprites.pl').
:- ensure_loaded('rules.pl').

% # Draw

clear_screen:- 
    write('\33\[2J').

draw_title :-
    write('   ______     __                                                                            \n'),
    write('  /      \\   /  |                                                                           \n'),
    write(' /$$$$$$  | _$$ |_     ______   _______    ______    _______                                \n'),
    write(' $$ \\__$$/ / $$   |   /      \\ /       \\  /      \\  /       |                               \n'),
    write(' $$      \\ $$$$$$/   /$$$$$$  |$$$$$$$  |/$$$$$$  |/$$$$$$$/                                \n'),
    write('  $$$$$$  |  $$ | __ $$ |  $$ |$$ |  $$ |$$    $$ |$$      \\                                \n'),
    write(' /  \\__$$ |  $$ |/  |$$ \\__$$ |$$ |  $$ |$$$$$$$$/  $$$$$$  |                               \n'),
    write(' $$    $$/   $$  $$/ $$    $$/ $$ |  $$ |$$       |/     $$/                                \n'),
    write('  $$$$$$/     $$$$/   $$$$$$/  $$/   $$/  $$$$$$$/ $$$$$$$/                                 \n'),
    write('                                                                                           \n'),
    write('                                                                                           \n'),
    write('                                                                                           \n'),
    write('                            __        _______   __                                           \n'),
    write('                           /  |      /       \\ /  |                                          \n'),
    write('   ______   _______    ____$$ |      $$$$$$$  |$$/  __     __   ______    ______    _______ \n'),
    write('  /      \\ /       \\  /    $$ |      $$ |__$$ |/  |/  \\   /  | /      \\  /      \\  /       |\n'),
    write('  $$$$$$  |$$$$$$$  |/$$$$$$$ |      $$    $$< $$ |$$  \\ /$$/ /$$$$$$  |/$$$$$$  |/$$$$$$$/ \n'),
    write('  /    $$ |$$ |  $$ |$$ |  $$ |      $$$$$$$  |$$ | $$  /$$/  $$    $$ |$$ |  $$/ $$      \\ \n'),
    write(' /$$$$$$$ |$$ |  $$ |$$ \\__$$ |      $$ |  $$ |$$ |  $$ $$/   $$$$$$$$/ $$ |       $$$$$$  |\n'),
    write(' $$    $$ |$$ |  $$ |$$    $$ |      $$ |  $$ |$$ |   $$$/    $$       |$$ |      /     $$/ \n'),
    write('  $$$$$$$/ $$/   $$/  $$$$$$$/       $$/   $$/ $$/     $/      $$$$$$$/ $$/       $$$$$$$/  \n'),
    nl,nl.

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
    draw_vertical_connection_line(Width),
    nl,
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

draw_pieces([], _) :- !.

draw_pieces([Piece | Pieces], mid) :-
    !,
    piece_sprite(Piece, mid, T),
    format('- ~s -', [T]),
    draw_pieces(Pieces, mid).

draw_pieces([Piece | Pieces], Pos) :-
    piece_sprite(Piece, Pos, T),
    format('  ~s  ', [T]),
    draw_pieces(Pieces, Pos).

draw_row(Row, Y) :-
    Y // 10 =:= 0, !,
    length(Row, Width),
    draw_vertical_connection_line(Width),
    write('     '),         draw_pieces(Row, top),      nl,     
    format('~d   -', [Y]),  draw_pieces(Row, mid),      format('-   ~d', [Y]), nl,
    write('     '),         draw_pieces(Row, bottom),   nl.

draw_row(Row, Y) :-
    length(Row, Width),
    draw_vertical_connection_line(Width),
    write('     '),         draw_pieces(Row, top),      nl,     
    format('~d  -', [Y]),  draw_pieces(Row, mid),      format('-   ~d', [Y]), nl,
    write('     '),         draw_pieces(Row, bottom),   nl.

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