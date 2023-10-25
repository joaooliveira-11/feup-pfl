:- dynamic gamemode/1, boardsize/1, board/1. 
gamemode(normal).
boardsize(8).
board([]).
change_turn(p1,p2).
change_turn(p2,p1).
symbol(0, ' ') :- !.  % Empty square
symbol(1, 'B').       % Player 1
symbol(2, 'W').       % Player 2

cs :- write('\33\[2J').

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


print_dash_line(0).
print_dash_line(N) :-
    N > 0,
    write('----'),
    N1 is N - 1,
    print_dash_line(N1).


print_letters_columns(0).
print_letters_columns(N) :-
    N > 0,
    print_column_letter(N),
    N1 is N - 1,
    print_letters_columns(N1).

print_column_letter(N) :-
    N1 is 65 + N - 1, 
    char_code(Letter, N1), 
    format('~|~w | ', [Letter]).

print_matrix([], _).
print_matrix([Line|RestOfMatrix], Row) :-
    format('~|~t~d~3+ | ', [Row]),
    print_line(Line),
    nl,
    print_underscore_line,
    nl,
    NewRow is Row + 1, % Increase the row number
    print_matrix(RestOfMatrix, NewRow). % Recurse with the updated row number

print_underscore_line :-
    boardsize(SIZE), % Obtemos o tamanho do tabuleiro
    Length is SIZE * 4 + 4, % Calculamos o comprimento da linha de underscores
    format('~|~`-t~*|', [Length]). % Imprimimos a linha de underscores

% print_line(+Line)
% Displays each element of the line recursively
print_line([]).
print_line([CurrentElement|RestOfLine]) :-
    symbol(CurrentElement, Symbol),
    write(Symbol),
    write(' | '),
    print_line(RestOfLine).
