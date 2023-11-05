:- dynamic gamemode/1, boardsize/1, bot_level/1, first_move/1, can_continuous_move/2, black_blocked_positions/1 , white_blocked_positions/1, jump_piece/2.
gamemode(h/h).
boardsize(8).
bot_level(1).

change_turn('W','B').
change_turn('B','W').
symbol(0, ' ') :- !.  % Empty square
symbol(1, 'B').       % Player 1
symbol(2, 'W').       % Player 2
means('W', white).
means('B', black).

can_continuous_move('W', no).
can_continuous_move('B', no).
first_move('W').
white_blocked_positions([]).
black_blocked_positions([]).
jump_piece('W',-1-1).
jump_piece('B', -1-1).

% cs/0
% Clear screen
cs :- write('\33\[2J'), nl.

% display_mainMenu/0
% Display main menu with the game modes
display_mainMenu :- 
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                   |------- Game Mode Menu ------|                   |\n'),
    write('|                                                                     |\n'),
    write('|                        1. Human vs Human                            |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Human vs Computer                         |\n'),
    write('|                                                                     |\n'),
    write('|                        3. Computer vs Human                         |\n'),
    write('|                                                                     |\n'),
    write('|                        4. Computer vs Computer                      |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

% display_boardsizeMenu/0
% Display board size menu for players to choose the board size
display_boardsizeMenu :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                   |------ Board Size Menu ------|                   |\n'),
    write('|                                                                     |\n'),
    write('|                        1. 8x8 (Recomended)                          |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Other                                     |\n'),
    write('|                                                                     |\n'),
    write('|                        0. Game Mode Menu                            |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

% display_playerMenu/0
% Display player menu for players to choose their pieces
display_playerMenu :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                   |-------- Player Menu --------|                   |\n'),
    write('|                                                                     |\n'),
    write('|                        1. Player 1 - B                              |\n'),
    write('|                                                                     |\n'),
    write('|                        2. Player 2 - W (Starts First)               |\n'),
    write('|                                                                     |\n'),
    write('|                        0. Game Mode Menu                            |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

% display_botMenu/0
% Display bot menu with the game modes
display_botMenu :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                   |---------- Bot Menu ---------|                   |\n'),
    write('|                                                                     |\n'),
    write('|                      1. Level 1 (Random Moves)                      |\n'),
    write('|                                                                     |\n'),
    write('|                      2. Level 2 (Greedy Moves)                      |\n'),
    write('|                                                                     |\n'),
    write('|                      0. Game Mode Menu                              |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

% display_gamewinMenu/0
% Display the winner
display_gamewin(WINNER) :-
    write(' _____________________________________________________________________\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                   |  A  |  P  |  A  |  R  |  T  |                   |\n'),
    write('|                   |-----+-----+-----+-----+-----|                   |\n'),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|                           G A M E   W I N                           |\n'),
    format('|                        Winner: ~w', [WINNER]                           ),
    write('|                                                                     |\n'),
    write('|                                                                     |\n'),
    write('|_____________________________________________________________________|\n').

% print_dash_line(+N)
% Auxiliar function to display the board
print_dash_line(0).
print_dash_line(N) :-
    N > 0,
    write('----'),
    N1 is N - 1,
    print_dash_line(N1).

% print_columns(+N)
% Auxiliar function to display the board with column numbers in ascending order starting from 1
print_columns(N) :-
    print_columns(0, N).

% print_columns(+CURRENT, +N)
print_columns(N, N).
print_columns(Current, N) :-
    Next is Current + 1,
    format('~|~d | ', [Next]),
    print_columns(Next, N).

% print_matrix(+BOARD, +ROW)
% Auxiliar function to display the board
print_matrix([], _).
print_matrix([Line|RestOfMatrix], Row) :-
    format('~|~t~d~3+ | ', [Row]),
    print_line(Line),
    nl,
    print_underscore_line,
    nl,
    NewRow is Row + 1,
    print_matrix(RestOfMatrix, NewRow). 

% print_underscore_line/0
% % Auxiliar function to display the board
print_underscore_line :-
    boardsize(SIZE), 
    Length is SIZE * 4 + 4, 
    format('~|~`-t~*|', [Length]). 

% print_line(+Line)
% % Auxiliar function to display the board
print_line([]).
print_line([CurrentElement|RestOfLine]) :-
    symbol(CurrentElement, Symbol),
    write(Symbol),
    write(' | '),
    print_line(RestOfLine).

% print_player_turn(+PLAYER)
% Prints the player turn.
print_player_turn(PLAYER):-
    means(PLAYER, NAME),
    format('~w pieces turn\n: ', [NAME]).


board_checkwin(BOARD) :-
    BOARD = [
        [0, 0, 0, 0, 0, 0, 0, 0],
        [2, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 2, 0, 0, 0],
        [2, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 2, 2, 0, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 2, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0]
  ].

print_moves([]).
print_moves([[StartX-StartY, EndX-EndY] | Rest]) :-
    format('Move from ~w-~w to ~w-~w\n', [StartX, StartY, EndX, EndY]),
    print_moves(Rest).

print_isolated_positions([]).
print_isolated_positions([[Y, X] | Rest]) :-
    format("(~d-~d),", [Y, X]),
    print_isolated_positions(Rest).

print_sorted_moves([]).
print_sorted_moves([Value-Move | Rest]) :-
    [YS-XS, YF-XF] = Move,
    format("Move from ~w-~w to ~w-~w, Value: ~w~n", [YS,XS, YF,XF, Value]),
    print_sorted_moves(Rest).

print_positions([]).
print_positions([Position | Rest]) :-
    format('~w-~w ', Position),
    print_positions(Rest).