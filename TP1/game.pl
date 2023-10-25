:- use_module(library(lists)).

build_piecerow(SIZE, PIECE, ROW) :-
    EMPTYSIZE is SIZE -2,
    length(ROW_AUX, EMPTYSIZE),
    maplist(=(PIECE), ROW_AUX),
    ROW_AUX1 = [0 | ROW_AUX],
    append(ROW_AUX1, [0], ROW).

build_normalrow(SIZE, ROW) :-
    length(ROW, SIZE),
    maplist(=(0), ROW).

build_rows(_, 0, BOARD, BOARD).
build_rows(SIZE, NROWS, BOARD, RESULTBOARD) :-
    NROWS > 0,
    build_normalrow(SIZE, ROW),
    append(BOARD, [ROW], UPDATEDBOARD),
    NROWS1 is NROWS - 1,
    build_rows(SIZE, NROWS1, UPDATEDBOARD, RESULTBOARD).

initial_state(SIZE, BOARD) :-
    build_piecerow(SIZE, 1, FIRSTROW),
    build_piecerow(SIZE, 1, SECONDROW),
    build_piecerow(SIZE, 2, SECONDLASTROW),
    build_piecerow(SIZE, 2, LASTROW),

    BOARD_AUX = [FIRSTROW, SECONDROW],
    MIDDLESIZE is SIZE - 4,
    build_rows(SIZE, MIDDLESIZE, [], MIDROWS),

    append(BOARD_AUX, MIDROWS, UPDATEDBOARD),
    append(UPDATEDBOARD, [SECONDLASTROW, LASTROW], BOARD),

    retract(board(_)),
    assert(board(BOARD)).

% Exibe o tabuleiro
display_game(BOARD) :-
    boardsize(SIZE),
    write('    | '),
    print_letters_columns(SIZE), nl,
    write('    '),
    print_dash_line(SIZE),
    nl,
    print_matrix(BOARD, 1).

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

symbol(0, ' ') :- !.  % Empty square
symbol(1, 'B').       % Player 1
symbol(2, 'W').       % Player 2

play_game :-
    boardsize(SIZE),
    initial_state(SIZE, BOARD),
    display_game(BOARD).
