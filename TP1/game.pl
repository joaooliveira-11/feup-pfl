:- dynamic gamemode/1, boardSize/1.
gamemode(normal).
boardSize(8).


% Inicializa o tabuleiro com as peças iniciais
initial_state(Board) :-
  Board = [
    [0, 1, 1, 1, 1, 1, 1, 0],
    [0, 1, 1, 1, 1, 1, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 2, 2, 2, 2, 2, 2, 0],
    [0, 2, 2, 2, 2, 2, 2, 0]
  ].

% Exibe o tabuleiro
display_game(Board) :-
    nl,
    write('   1   2   3   4   5   6   7   8  '), nl,
    write('  --------------------------------'), nl,
    print_matrix(Board, 8).

% print_matrix(+Board, +Row)
% Displays each row of the board recursively
print_matrix([], _).
print_matrix([Line|RestOfMatrix], Row) :-
    write(Row),
    write(' | '),
    print_line(Line),
    nl,
    write('  --------------------------------'), nl,
    NewRow is Row - 1,
    print_matrix(RestOfMatrix, NewRow).

% print_line(+Line)
% Displays each element of the line recursively
print_line([]).
print_line([CurrentElement|RestOfLine]) :-
    symbol(CurrentElement, Symbol),
    write(Symbol),
    write(' | '),
    print_line(RestOfLine).

symbol(0, ' ') :- !.  % Empty square
symbol(1, '1').       % Player 1
symbol(2, '2').       % Player 2

play_game :-
    initial_state(Board),
    display_game(Board).
