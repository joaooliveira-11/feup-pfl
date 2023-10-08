:- consult('game.pl').

main_menu :-
    print_main_menu,
    read(Input),
    manage_option(Input).

print_main_menu :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                              1. Play                                |\n'),
    write('|                                                                     |\n'),
    write('|                              0. Exit                                |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n'),
    write(' Insert your option:\n').

% Manage the option selected by the player and act upon it 
% Start a Player versus Player game
% manage_option(+Option)
manage_option(1) :-
      play_game.
      %main_menu.

manage_option(0) :-
      write('\nExit. We hope to see you again :)\n\n').

manage_option(_) :-
write("Invalid option. Please choose a valid option.\n"),
main_menu.
