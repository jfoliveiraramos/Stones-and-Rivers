:- dynamic emptyBoard/1.

% get_empty_board(-Board)
% Returns the empty board to be used in the game.
get_empty_board(Board) :- emptyBoard(Board).

% set_empty_board(+Board)
% Sets the empty board to be used in the game.
set_emptyBoard(Board) :-
    retractall(emptyBoard(_)),
    assert(emptyBoard(Board)).

:- dynamic player_level/2.

% player_level(?Player, ?Level)
% Defines the level of the player.
player_level(player_a, human).
player_level(player_b, human).

% is_human(+Player)
% Checks if the player is human.
is_human(Player) :- player_level(Player, human).

% is_bot(+Player)
% Checks if the player is a bot.
is_bot(Player) :- \+ is_human(Player).

% is_easy(+Player)
% Checks if the player is a bot of level 1.
is_easy(Player) :- player_level(Player, level1).

% is_hard(+Player)
% Checks if the player is a bot of level 2.
is_hard(Player) :- player_level(Player, level2).

% get_players(-Players)
% Returns the players.
get_players(PlayerA/PlayerB) :-
    player_level(player_a, PlayerA),
    player_level(player_b, PlayerB).

% set_player(+Player, +Type)
% Sets the player to the given type.
set_player(Player, Type) :-
    retractall(player_level(Player, _)),
    assert(player_level(Player, Type)).

% set_players(+Players)
% Sets the players to the given types.
set_players(PlayerA/PlayerB) :-
    set_player(player_a, PlayerA),
    set_player(player_b, PlayerB).

:- dynamic size/2.

% size(?Width, ?Height)
% Defines the size of the board.
size(13,14).

% set_size(+Size)
% Sets the size of the board.
set_size(Width/Height) :-
    retractall(size(_,_)),
    assert(size(Width, Height)).

% get_size(-Size)
% Returns the size of the board.
get_size(Width/Height) :- size(Width, Height).

% settings(-Settings)
% Returns the settings of the game.
settings(Size-Players) :-
    get_size(Size),
    get_players(Players).

