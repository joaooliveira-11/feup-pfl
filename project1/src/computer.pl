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

choose_move(GAMESTATE, PLAYER, 1, MOVE) :-
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    random_choice(MOVE, VALIDMOVES),
    MOVE = [YS-XS, YF-XF],
    format('Computer randomly moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]),nl.

choose_move(GAMESTATE, PLAYER, 2, MOVE) :-
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    choose_best_move(GAMESTATE, PLAYER, VALIDMOVES, MOVE),
    wait_execute,
    % sleep(3),
    [YS-XS, YF-XF] = MOVE,
    format('Computer greedily moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]), nl.

choose_best_move(GAMESTATE, PLAYER, VALIDMOVES, MOVE) :-
    findall(
        VALUE-MOVE,
        (
            member(MOVE, VALIDMOVES),
            execute_move(GAMESTATE, MOVE, NEWGAMESTATE),
            value(NEWGAMESTATE, PLAYER, VALUE)
        ),
        MOVESVALUE
    ),
    keysort(MOVESVALUE, SORTEDMOVESVALUE),
    last(SORTEDMOVESVALUE, MAX-_),
    findall(MOVES, member(MAX-MOVES, SORTEDMOVESVALUE), MAXMOVES),
    random_choice(MOVE, MAXMOVES),
    print_sorted_moves(SORTEDMOVESVALUE).  

get_computer_answer(GAMESTATE, _, 1) :-
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

get_computer_answer(GAMESTATE, PLAYER, 2) :-
    write('Since you made a jump and the jumped piece can move again, you are allowed to play again.\n'),
    write('Do you want to play again (yes or no)?\n'),
    choose_best_answer(GAMESTATE, PLAYER, ANSWER),
    format('computer greedily chose: ~w.\n', [ANSWER]),
    (
    ANSWER = 'yes' ->
        play_game(GAMESTATE)
    ;
    ANSWER = 'no' ->
        [BOARD, SIZE, _, GAMEMODE, BOTLEVEL] = GAMESTATE,
        allow_single_steps(PLAYER),
        clear_blocked_positions(PLAYER),
        remove_continousmove_piece(PLAYER),
        change_turn(PLAYER, NEXTPLAYER),
        NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL],    
        play_game(NEWGAMESTATE)
    ).

choose_best_answer(GAMESTATE, PLAYER, ANSWER) :-
    value(GAMESTATE, PLAYER, CURRENTVALUE),
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    choose_best_move(GAMESTATE, PLAYER, VALIDMOVES, MOVE),
    execute_move(GAMESTATE, MOVE, NEWGAMESTATE),
    value(NEWGAMESTATE, FUTUREVALUE),
    (FUTUREVALUE > CURRENTVALUE ->
        ANSWER = 'yes'
    ;
        ANSWER = 'no'
    ).

