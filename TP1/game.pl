:- use_module(library(lists)).
:- consult(data).
:- consult(logic).

% build_piecerow(+SIZE, +PIECE, -ROW)
% Predicate to build a row with  SIZE-2 pieces.
build_piecerow(SIZE, PIECE, ROW) :-
    EMPTYSIZE is SIZE -2,
    length(ROW_AUX, EMPTYSIZE),
    maplist(=(PIECE), ROW_AUX),
    ROW_AUX1 = [0 | ROW_AUX],
    append(ROW_AUX1, [0], ROW).

% build_normalrow(+SIZE, -ROW)
% Predicate to build an empty row
build_normalrow(SIZE, ROW) :-
    length(ROW, SIZE),
    maplist(=(0), ROW).

% build_rows(+SIZE, +NROWS, +BOARD, -RESULTBOARD)
% Predicate to build the middle rows of the board.
build_rows(_, 0, BOARD, BOARD).
build_rows(SIZE, NROWS, BOARD, RESULTBOARD) :-
    NROWS > 0,
    build_normalrow(SIZE, ROW),
    append(BOARD, [ROW], UPDATEDBOARD),
    NROWS1 is NROWS - 1,
    build_rows(SIZE, NROWS1, UPDATEDBOARD, RESULTBOARD).

% initial_state(+SIZE, -BOARD)
% Predicate to build a board (size x size) given a size.
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

% display_game(+BOARD)
% Predicate to display the board.
display_game(BOARD) :-
    boardsize(SIZE),
    write('    | '),
    print_letters_columns(SIZE), nl,
    write('    '),
    print_dash_line(SIZE),
    nl,
    print_matrix(BOARD, 1).

% move(+GAMESTATE, +PLAYER, +MOVE, -NEWGAMESTATE)
% Predicate to analise and execute a move from a player.
move(GAMESTATE, PLAYER, [START, END], NEWGAMESTATE) :-
    get_direction(START, END, DIRECTION),
    get_piece(GAMESTATE, START, PIECE),
    get_piece(GAMESTATE, END, FPIECE),
    get_move_linelength(GAMESTATE, START, PIECE, DIRECTION, LINELENGTH),
    get_move_length([START, END], LENGTH),
    valid_move([START, END], PLAYER, PIECE, FPIECE, LENGTH, LINELENGTH, DIRECTION, TYPE),
    execute_move(GAMESTATE, START, END, NEWGAMESTATE),
    handle_move_type(TYPE, PLAYER, END),
    add_blocked_position(PLAYER, START).

play :-
    boardsize(SIZE),
    %initial_state(SIZE, BOARD),
    board_checkwin(BOARD),
    display_game(BOARD),
    play_game(BOARD, 'W').

play_game(GAMESTATE, PLAYER) :-
    get_move(PLAYER,MOVE),
    (
        move(GAMESTATE, PLAYER, MOVE, NEWGAMESTATE),
        display_game(NEWGAMESTATE),
        (game_over(NEWGAMESTATE, PLAYER, WINNER) ->
            gamewin_menu(WINNER)
        ;
            continue_game(NEWGAMESTATE, PLAYER)
        )
    ;
        play_game(GAMESTATE, PLAYER)
    ).
