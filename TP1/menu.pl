:- consult(io).
:- consult(data).

display_menu(MENU) :-
    cs, 
    write(MENU).

main_menu :-
    mainmenu_data(DATA),
    display_menu(DATA),
    get_menuinput(1, 3, INPUT),
    set_gamemode(INPUT),
    boardsize_menu.

boardsize_menu :-
    boardmenu_data(DATA),
    display_menu(DATA),
    get_menuinput(0, 3, INPUT),
    set_boardsize(INPUT),
    % player_menu.
    play_game.

/*
player_menu :-
    playermenu_data(DATA),
    display_menu(DATA)
    input_handler(0, 2, INPUT),
    set_playerside(INPUT).
    */

    