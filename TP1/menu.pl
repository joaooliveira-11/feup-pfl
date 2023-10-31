:- consult(io).
:- consult(data).
:- use_module(library(between)).

% display_menu/0
% Display the input menu
display_menu(MENU) :-
    write(MENU).

% main_menu/0
% Displays the main menu
% Asks user to choose the game mode
% Sets the game mode
% Moves to next menu
main_menu :-
    display_mainMenu,
    get_menuinput(1, 3, INPUT),
    set_gamemode(INPUT),
    boardsize_menu.

% boardsize_menu
% Displays the board size menu
% Asks user to choose the board size
% Sets the board size
% Moves to next menu
boardsize_menu :-
    display_boardsizeMenu,
    get_menuinput(0, 2, INPUT),
    (
        INPUT = 2 ->
        (
            get_menuinput(8, 25, BOARDSIZE),
            set_boardsize(2, BOARDSIZE)            
        );

        INPUT = 1 ->
        (
            set_boardsize(1, BOARDSIZE)
        );

        INPUT = 0 ->
        (
            set_boardsize(0, BOARDSIZE)
        )
    ),
    play.


/*
player_menu :-
    display_playerMenu,
    get_menuinput(0, 2, INPUT),
    set_playerside(INPUT).
*/

gamewin_menu(WINNER):-
    display_gamewin(WINNER).