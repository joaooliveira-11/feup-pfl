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
    get_move_maxlength(GAMESTATE, START, PIECE, DIRECTION, MAXLENGTH),
    get_move_length([START, END], LENGTH),
    valid_move(END, PLAYER, PIECE, FPIECE, LENGTH, MAXLENGTH, DIRECTION, TYPE),
    execute_move(GAMESTATE, START, END, NEWGAMESTATE),
    handle_move_type(TYPE, PLAYER),
    add_blocked_position(PLAYER, START).

play :-
    boardsize(SIZE),
    initial_state(SIZE, BOARD),
    display_game(BOARD),
    play_game(BOARD, 'W').

play_game(GAMESTATE, PLAYER) :-
    get_move(PLAYER,MOVE),
    (
        move(GAMESTATE, PLAYER, MOVE, NEWGAMESTATE),
        display_game(NEWGAMESTATE),
        boardsize(SIZE),
        get_player_positions(NEWGAMESTATE, PLAYER, SIZE, POSITIONS),
        print_positions(POSITIONS), nl,
        
        (
        check_white_first_move(PLAYER) ->
            allow_single_steps(PLAYER),
            change_turn(PLAYER, NEXTPLAYER),
            clear_blocked_positions(PLAYER),
            play_game(NEWGAMESTATE, NEXTPLAYER)
        ;
        can_continuous_move(PLAYER, yes) ->
            ask_to_play_again(NEWGAMESTATE, PLAYER)
        ;
        can_continuous_move(PLAYER, no) ->
            change_turn(PLAYER, NEXTPLAYER),
            clear_blocked_positions(PLAYER),
            play_game(NEWGAMESTATE, NEXTPLAYER)
        )
    ;
        play_game(GAMESTATE, PLAYER)
    ).
