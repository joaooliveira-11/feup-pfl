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

% build_board(+SIZE, -BOARD)
% Predicate to build a board (size x size) given a size.
build_board(SIZE, BOARD) :-
    build_piecerow(SIZE, 1, FIRSTROW),
    build_piecerow(SIZE, 1, SECONDROW),
    build_piecerow(SIZE, 2, SECONDLASTROW),
    build_piecerow(SIZE, 2, LASTROW),

    BOARD_AUX = [FIRSTROW, SECONDROW],
    MIDDLESIZE is SIZE - 4,
    build_rows(SIZE, MIDDLESIZE, [], MIDROWS),

    append(BOARD_AUX, MIDROWS, UPDATEDBOARD),
    append(UPDATEDBOARD, [SECONDLASTROW, LASTROW], BOARD).

% initial_state(+SIZE, -GAMESTATE)
% Predicate that receives the board size as an argument and returns an initial game state.  
initial_state(SIZE, GAMESTATE) :-
    build_board(SIZE, BOARD),
    gamemode(GAMEMODE),
    bot_level(BOTLEVEL),
    GAMESTATE = [BOARD, SIZE, 'W', GAMEMODE, BOTLEVEL].

% display_game(+GAMESTATE)
% Predicate to display the board given a game state.
display_game(GAMESTATE) :-
    [BOARD, SIZE, _, _,_] = GAMESTATE,
    write('    | '),
    print_columns(SIZE), nl,
    write('    '),
    print_dash_line(SIZE),
    nl,
    print_matrix(BOARD, 1).

% move(+GAMESTATE, +MOVE, -NEWGAMESTATE)
% Predicate responsible for move validation and execution.
% Returns a new game state with the updated board.
move(GAMESTATE, [START, END], NEWGAMESTATE) :-
    [BOARD,_, PLAYER, _, _] = GAMESTATE,
    valid_move(BOARD, PLAYER, [START, END], TYPE, 1),
    execute_move(GAMESTATE, [START, END], NEWGAMESTATE),
    handle_move_type(TYPE, PLAYER, [START, END]).

/*
% prepare_game/0
% Predicate that prepares the configurations before starting the game cycle.
prepare_game :-
    cs,
    boardsize(SIZE),
    initial_state(SIZE, GAMESTATE),
    change_random_seed,
    display_game(GAMESTATE), nl,
    play_game(GAMESTATE).
*/

% play_game(+GAMESTATE)
% Predicate that represents the game cycle.
play_game(GAMESTATE) :-
    [_,_,_, GAMEMODE,_] = GAMESTATE,
    get_move(GAMESTATE,GAMEMODE,MOVE),
    (   
        move(GAMESTATE, MOVE, NEWGAMESTATE),
        display_game(NEWGAMESTATE),
        (game_over(NEWGAMESTATE, WINNER) ->
            gamewin_menu(WINNER)
        ;
            continue_game(NEWGAMESTATE)
        )
    ;
        play_game(GAMESTATE)
    ).

prepare_game :-
    cs,
    boardsize(SIZE),
    gamemode(GAMEMODE),
    % initial_state(SIZE, BOARD),
    bot_level(BOTLEVEL),
    board_checkwin(BOARD),
    change_random_seed,
    display_game([BOARD, SIZE, 'W',GAMEMODE, BOTLEVEL]), nl,
    play_game([BOARD, SIZE, 'W',GAMEMODE, BOTLEVEL]).
