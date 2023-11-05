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
    nl, nl.

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

player(human, 'Human') :- !.
player(level1, 'Bot (Easy)') :- !.
player(level2, 'Bot (Hard)') :- !.

player_type(Player, 'Human') :- is_human(Player), !.
player_type(Player, 'Bot') :- is_bot(Player), !.

player_to_text(player_a, 'Player A') :- !.
player_to_text(player_b, 'Player B') :- !.

draw_turn(Player):-
    player_to_text(Player, PlayerText),
    player_type(Player, Type),
    format('~w\'s turn - ~w\n\n', [PlayerText, Type]).

display_settings(Width/Height-A/B) :-
    write('Current settings\n\n'),
    format('Board size: ~d by ~d\n', [Width, Height]),
    player(A, PlayerA),
    player(B, PlayerB),
    format('Player A: ~w\n', [PlayerA]),
    format('Player B: ~w\n\n\n', [PlayerB]).

display_menu_options :-
    write('Choose an option:\n\n'),
    write('1. Start game\n'),
    write('2. Change players\n'),
    write('3. Change board size\n'),
    write('0. Quit\n'),
    nl.

draw_winner(Winner) :-
    player_to_text(Winner, WinnerText),
    format('Congratulations! ~w won the game!\n\n', [WinnerText]).

display_gameOver_options :-
    write('Choose an option:\n\n'),
    write('1. Play again\n'),
    write('2. Quit to main menu\n\n').
