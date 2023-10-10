:- consult(io).

main_menu :-
    display_mainMenu,
    input_handler(1, 3, Option),
    handle_mainMenu(Option).

boardsize_menu :-
    display_boardsizeMenu,
    input_handler(0, 2, Option),
    handle_boardsizeMenu(Option).

/*
manage_option(1) :-
      play_game.
      %main_menu.

manage_option(0) :-
      write('\nExit. We hope to see you again :)\n\n').

manage_option(_) :-
write("Invalid option. Please choose a valid option.\n"),
main_menu.
*/



display_mainMenu :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                        Main Menu                                    |\n'),
    write('|                                                                     |\n'),
    write('|                        Select a Game Mode:                          |\n'),
    write('|                                                                     |\n'),
    write('|                        1. Human vs Human                            |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Human vs Computer                         |\n'),
    write('|                                                                     |\n'),
    write('|                        3. Computer vs Computer                      |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

display_boardsizeMenu :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                        Board Size Menu                              |\n'),
    write('|                                                                     |\n'),
    write('|                        Select a Board Size:                         |\n'),
    write('|                                                                     |\n'),
    write('|                        1. 8x8 (Recomended)                          |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Other                                     |\n'),
    write('|                                                                     |\n'),
    write('|                        0. Main Menu                                 |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

display_playerMenu :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                        Player Menu                                  |\n'),
    write('|                                                                     |\n'),
    write('|                        1. Player 1 - W                              |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Player 2 - B                              |\n'),
    write('|                                                                     |\n'),
    write('|                        0. Main Menu                                 |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').