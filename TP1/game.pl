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

build_board(SIZE, BOARD) :-
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
display_game(BOARD, SIZE) :-
    nl,
    write('   1   2   3   4   5   6   7   8  '), nl,
    write('  --------------------------------'), nl,
    print_matrix(BOARD, SIZE).

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
symbol(1, 'B').       % Player 1
symbol(2, 'W').       % Player 2

play_game :-
    boardsize(SIZE),
    build_board(SIZE, BOARD),
    display_game(BOARD, SIZE).
