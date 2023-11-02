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

draw_board(Board) :-
    board_size(Board, W, H),
    draw_top_x_axis(W),
    draw_rows(Board, H, 0),
    draw_bottom_x_axis(W).

player(hum, 'Human') :- !.
player(pc1, 'Computer (Easy)') :- !.
player(pc2, 'Computer (Hard)') :- !.

player_type(player_a, A/_, Type) :- player(A, Type), !.
player_type(player_b, _/B, Type) :- player(B, Type), !.

turn_to_text(player_a, 'Player A') :- !.
turn_to_text(player_b, 'Player B') :- !.

draw_turn(Turn, Players):-
    turn_to_text(Turn, TurnText),
    player_type(Turn, Players, Type),
    format('~w\'s turn - ~w\n\n', [TurnText, Type]).

display_game(Turn-Board-Players) :-
    clear_screen,
    draw_board(Board),
    draw_turn(Turn, Players),
    nl.

display_settings(Width/Height-A/B) :-
    write('Current settings:\n\n'),
    format('Board size: ~d by ~d\n', [Width, Height]),
    player(A, PlayerA),
    player(B, PlayerB),
    format('Player A: ~w\n', [PlayerA]),
    format('Player B: ~w\n\n', [PlayerB]).