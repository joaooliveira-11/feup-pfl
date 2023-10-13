is_list_of_lists(Term) :-
    is_list(Term),        
    forall(member(Element, Term), is_list(Element)).

build_piecerow(SIZE, PIECE, ROW) :-
    EMPTYSIZE is SIZE - 2,
    length(ROW_AUX, EMPTYSIZE),
    maplist(=(PIECE), ROW_AUX),
    ROW_AUX1 = [0 | ROW_AUX],
    append(ROW_AUX1, [0], ROW).

build_normalrow(SIZE, ROW) :-
    length(ROW, SIZE),
    maplist(=(0), ROW).

build_rows(_, 0, Board, Board).
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

    MIDDLESIZE is SIZE - 4,
    build_rows(SIZE, MIDDLESIZE, [], MIDROWS),

    append([FIRSTROW, SECONDROW], MIDROWS, INTERMEDIATEBOARD),
    append(INTERMEDIATEBOARD, [SECONDLASTROW, LASTROW], BOARD).


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
symbol(1, '1').       % Player 1
symbol(2, '2').       % Player 2

play_game :-
    boardsize(SIZE),
    build_board(SIZE, BOARD),
    display_game(BOARD, SIZE).
