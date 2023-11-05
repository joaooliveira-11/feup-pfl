:- consult(io).
:- consult(data).
:- consult(game).
:- use_module(library(between)).

% main_menu/0
% Displays the main menu.
% Asks user to choose the game mode.
% Sets the game mode.
% Moves to next menu.
main_menu :-
    display_mainMenu,
    get_menuinput(1, 4, INPUT),
    set_gamemode(INPUT),
    boardsize_menu.

% boardsize_menu/0
% Displays the board size menu.
% Asks user to choose the board size.
% Sets the board size.
% Moves to next menu.
boardsize_menu :-
    display_boardsizeMenu,
    get_menuinput(0, 2, INPUT),
    (INPUT = 2 ->
        get_menuinput(8, 25, BOARDSIZE),
        set_boardsize(2, BOARDSIZE)            
    ;
        set_boardsize(INPUT, _)
    ),
    gamemode(GAMEMODE),
    (GAMEMODE = h/h ->
        player_menu
    ;
        bot_level_menu
    ).

% player_menu/0
% Displays the player menu.
% Asks user to choose which side they want to play.
% Executes the prepare_game/1 predicate.
player_menu :-
    display_playerMenu,
    get_menuinput(0, 2, INPUT),
    set_playerside(INPUT),
    prepare_game.

% bot_level_menu/0
% Displays the bot menu.
% Asks user to choose the bot level.
% Executes the prepare_game/1 predicate.
bot_level_menu :-
    display_botMenu,
    get_menuinput(0, 2, INPUT),
    set_bot_level(INPUT),
    prepare_game.

% gamewin_menu(+WINNER)
% Displays the game win menu.
gamewin_menu(WINNER):-
    display_gamewin(WINNER).