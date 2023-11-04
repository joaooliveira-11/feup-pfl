:- use_module(library(random)).
:- use_module(library(system)).
:- consult(logic).
:- consult(game).

wait_execute :-
    repeat,
    (catch(read(OPTION), _, fail) ->
        OPTION = 'e'
    ;
        fail
    ).

change_random_seed :-
    now(TIMESTAMP),
    setrand(TIMESTAMP).

random_choice(CHOICE, OPTIONS) :-
    length(OPTIONS, LENGTH),
    random(0, LENGTH, INDEX),
    nth0(INDEX, OPTIONS, CHOICE).

choose_move(GAMESTATE, 1, MOVE) :-
    valid_moves(GAMESTATE, VALIDMOVES),
    random_choice(MOVE, VALIDMOVES),
    MOVE = [YS-XS, YF-XF],
    format('Computer randomly moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]),nl.

choose_move(GAMESTATE, 2, MOVE) :-
    valid_moves(GAMESTATE, VALIDMOVES),
    choose_best_move(GAMESTATE, VALIDMOVES, MOVE),
    wait_execute,
    % sleep(3)
    [YS-XS, YF-XF] = MOVE,
    format('Computer greedily moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]), nl.

choose_best_move(GAMESTATE, VALIDMOVES, MOVE) :-
    [BOARD, SIZE, PLAYER, _, _] = GAMESTATE,
    change_turn(PLAYER, NEXTPLAYER),
    findall(
        VALUE-MOVE,
        (
            member(MOVE, VALIDMOVES),
            get_player_positions(BOARD, NEXTPLAYER, SIZE, BEFOREMOVE),
            length(BEFOREMOVE, PIECESBEFORE),
            execute_move(GAMESTATE, MOVE, NEWGAMESTATE),
            [NEWBOARDBOARD, _, _, _, _] = NEWGAMESTATE,
            value(NEWGAMESTATE, BOARDVALUE),
            get_player_positions(NEWBOARDBOARD, NEXTPLAYER, SIZE, AFTERMOVE),
            length(AFTERMOVE, PIECESAFTER),
            TRADE_OFF_FACTOR is -2,
            ENEMY_PIECES_CAPTURED is PIECESBEFORE - PIECESAFTER,
            VALUE is BOARDVALUE + (TRADE_OFF_FACTOR * ENEMY_PIECES_CAPTURED)
        ),
        MOVESVALUE
    ),
    keysort(MOVESVALUE, TEMPSORT),
    reverse(TEMPSORT, SORTEDMOVESVALUE),
    [VALUE-MOVE | _] = SORTEDMOVESVALUE,
    print_sorted_moves(SORTEDMOVESVALUE).  

get_computer_answer(GAMESTATE, 1) :-
    write('Since you made a jump and the jumped piece can move again, you are allowed to play again.\n'),
    write('Do you want to play again (yes or no)?\n'),
    random_choice(ANSWER, ['yes', 'no']),
    format('computer randomly chose: ~w.\n', [ANSWER]),
    (
    ANSWER = 'yes' ->
        play_game(GAMESTATE)
    ;
    ANSWER = 'no' ->
        [BOARD, SIZE, PLAYER, GAMEMODE, BOTLEVEL] = GAMESTATE,
        allow_single_steps(PLAYER),
        clear_blocked_positions(PLAYER),
        remove_continousmove_piece(PLAYER),
        change_turn(PLAYER, NEXTPLAYER),
        NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL],    
        play_game(NEWGAMESTATE)
    ).

get_computer_answer(GAMESTATE, 2) :-
    choose_best_answer(GAMESTATE, ANSWER),
    format('computer greedily chose: ~w.\n', [ANSWER]),
    (
    ANSWER = 'yes' ->
        play_game(GAMESTATE)
    ;
    ANSWER = 'no' ->
        [BOARD, SIZE, PLAYER, GAMEMODE, BOTLEVEL] = GAMESTATE,
        allow_single_steps(PLAYER),
        clear_blocked_positions(PLAYER),
        remove_continousmove_piece(PLAYER),
        change_turn(PLAYER, NEXTPLAYER),
        NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL],    
        play_game(NEWGAMESTATE)
    ).

choose_best_answer(GAMESTATE, ANSWER) :-
    value(GAMESTATE, CURRENTVALUE),
    valid_moves(GAMESTATE, VALIDMOVES),
    choose_best_move(GAMESTATE, VALIDMOVES, MOVE),
    execute_move(GAMESTATE, MOVE, NEWGAMESTATE),
    value(NEWGAMESTATE, FUTUREVALUE),
    (FUTUREVALUE > CURRENTVALUE ->
        ANSWER = 'yes'
    ;
        ANSWER = 'no'
    ).

