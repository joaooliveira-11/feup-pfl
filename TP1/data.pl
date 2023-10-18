:- dynamic gamemode/1, boardsize/1, board/1. 
gamemode(normal).
boardsize(8).
board([]).
change_turn(p1,p2).
change_turn(p2,p1).

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
    write('|                        (Select a Game Mode:)                        |\n'),
    write('|                                                                     |\n'),
    write('|                        1. Human vs Human                            |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Human vs Computer                         |\n'),
    write('|                                                                     |\n'),
    write('|                        3. Computer vs Computer                      |\n'),
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
    write('|                        1. Player 1 - B                              |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Player 2 - W (Starts First)               |\n'),
    write('|                                                                     |\n'),
    write('|                        0. Main Menu                                 |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

