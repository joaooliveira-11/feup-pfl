:- use_module(library(lists)).
:- consult(data).
:- consult(logic).

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

% move(+GAMESTATE, +MOVE, +NEWGAMESTATE)
move(GAMESTATE, [START, END], NEWGAMESTATE) :-
    

    get_direction(START, END, DIRECTION),
    write('Your movement as direction: '), write(DIRECTION), nl,
    get_piece(GAMESTATE, START, PIECE),
    get_move_length(GAMESTATE, START, PIECE, DIRECTION, LENGTH),
    write('Your movement has a length of: '), write(LENGTH), write(' pieces'), nl,
    
    execute_move(GAMESTATE, START, END, NEWGAMESTATE).

play :-
    boardsize(SIZE),
    initial_state(SIZE, BOARD),
    display_game(BOARD),
    play_game(BOARD, p1).

play_game(GAMESTATE, PLAYER) :-
    get_move(MOVE),
    move(GAMESTATE, MOVE, NEWGAMESTATE),
    display_game(NEWGAMESTATE),
    % change_turn(PLAYER, NEXTPLAYER),    % Switch to the next player
    play_game(NEWGAMESTATE, NEXTPLAYER).
