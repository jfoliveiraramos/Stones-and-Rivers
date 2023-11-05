:- ensure_loaded('sprites.pl').
:- ensure_loaded('rules.pl').

% clear_screen/0
% Clears the screen.
clear_screen:- 
    write('\33\[2J').

% display_title/0
% Displays the game title.
display_title :-
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

% draw_x(+Width, +N)
% Draws the x axis with x value ranging from N to Width.
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

% draw_top_x_axis(+Width)
% Draws the top x axis.
draw_top_x_axis(Width) :-
    write('y/x  '),
    draw_x(Width, 0),
    nl,nl.

% draw_bottom_x_axis(+Width)
% Draws the bottom x axis.
draw_bottom_x_axis(Width) :-
    draw_vertical_connection_line(Width),
    nl,
    write('     '),
    draw_x(Width, 0),
    nl,nl.

% draw_vertical_connectors(+N)
% Draws a vertical connector '|' N times in the board grid.
draw_vertical_connectors(0) :- !.
draw_vertical_connectors(N) :-
    write('    |    '),
    N1 is N - 1,
    draw_vertical_connectors(N1).

% draw_vertical_connection_line(+N)
% Draws a vertical connection line for the board grid.
draw_vertical_connection_line(N) :-
    write('     '),
    draw_vertical_connectors(N),
    nl.

% draw_pieces(+Pieces, +Part)
% Draws a row of the pieces, for the specified part. 
% Part can be top, mid or bottom.
draw_pieces([], _) :- !.
draw_pieces([Piece | Pieces], mid) :-
    !,
    piece_sprite(Piece, mid, T),
    format('- ~s -', [T]),
    draw_pieces(Pieces, mid).
draw_pieces([Piece | Pieces], Part) :-
    piece_sprite(Piece, Part, T),
    format('  ~s  ', [T]),
    draw_pieces(Pieces, Part).

% draw_row(+Row, +Y)
% Draws a row of the board.
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

% draw_rows(+Rows, +Height, +Y)
% Draws the rows of the board. The number of rows is Height.
draw_rows([], Height, Height) :- !.
draw_rows([Row | Rows], Height, Y) :-
    draw_row(Row, Y),
    Y1 is Y + 1,
    draw_rows(Rows, Height, Y1).

% display_board(+Board)
% Displays the board.
display_board(Board) :-
    board_size(Board, W, H),
    draw_top_x_axis(W),
    draw_rows(Board, H, 0),
    draw_bottom_x_axis(W).

% player_text(?Player, ?PlayerText)
% Gets player and the respective text representation.
player_text(human, 'Human') :- !.
player_text(level1, 'Bot (Easy)') :- !.
player_text(level2, 'Bot (Hard)') :- !.

% player_type(?Player, ?Type)
% Gets player and the respective type text representation.
player_type(Player, 'Human') :- is_human(Player), !.
player_type(Player, 'Bot') :- is_bot(Player), !.

% turn_text(?PlayerTurn, ?PlayerTurnText)
% Gets player turn and the respective text representation.
turn_text(player_a, 'Player A') :- !.
turn_text(player_b, 'Player B') :- !.

% display_turn(+Player)
% Displays the turn text.
display_turn(Player):-
    turn_text(Player, PlayerText),
    player_type(Player, Type),
    format('~w\'s turn - ~w\n\n', [PlayerText, Type]).

% display_settings(+Settings)
% Displays the provided settings.
display_settings(Width/Height-A/B) :-
    write('Current settings\n\n'),
    format('Board size: ~d by ~d\n', [Width, Height]),
    player_text(A, PlayerA),
    player_text(B, PlayerB),
    format('Player A: ~w\n', [PlayerA]),
    format('Player B: ~w\n\n\n', [PlayerB]).

% display_menu_options/0
% Displays the main menu options.
display_menu_options :-
    write('Choose an option:\n\n'),
    write('1. Start game\n'),
    write('2. Change players\n'),
    write('3. Change board size\n'),
    write('0. Quit\n'),
    nl.

% display_winner(+Winner)
% Displays the winner announcement text.
display_winner(Winner) :-
    turn_text(Winner, WinnerText),
    format('Congratulations! ~w won the game!\n\n', [WinnerText]).

% display_gameOver_options/0
% Displays the game over options.
display_gameOver_options :-
    write('Choose an option:\n\n'),
    write('1. Play again\n'),
    write('2. Quit to main menu\n\n').
