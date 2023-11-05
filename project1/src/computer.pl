:- use_module(library(random)).
:- use_module(library(system)).
:- consult(logic).
:- consult(game).

% wait_execute/0
% Auxiliar predicate to execute a bot move by user input
wait_execute :-
    repeat,
    (catch(read(OPTION), _, fail) ->
        OPTION = 'e'
    ;
        fail
    ).

% auto_execute/0
% Auxiliar predicate to execute a bot move waiting 5 seconds.
auto_execute :-
    sleep(5).

% change_random_seed/0
% Changes the random/3 seed.
% Auxiliar predicate that helps random/3 get different options when having the same options.
change_random_seed :-
    now(TIMESTAMP),
    setrand(TIMESTAMP).

% random_choice(-CHOICE, +OPTIONS)
% Selects a random option from the options list.
% Auxilar predicate for bot level 1.
% Auxilar predicate for bot level 2 to select a random option from the options with the best score if more than 1 exist.
random_choice(CHOICE, OPTIONS) :-
    length(OPTIONS, LENGTH),
    random(0, LENGTH, INDEX),
    nth0(INDEX, OPTIONS, CHOICE).

% choose_move(+GAMESTATE, +PLAYER, +BOTLEVEL, -MOVE)
% Selects a move from a list of valid moves.
% If BOTLEVEL is 1, is a random choice.
% If BOTLEVEL is 2, is a random choice if more than 1 exist or the only one, from the options with the best value.
choose_move(GAMESTATE, PLAYER, 1, MOVE) :-
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    random_choice(MOVE, VALIDMOVES),
    MOVE = [YS-XS, YF-XF],
    format('Computer randomly moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]),nl.
choose_move(GAMESTATE, PLAYER, 2, MOVE) :-
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    choose_best_move(GAMESTATE, PLAYER, VALIDMOVES, MOVE),
    wait_execute,
    % auto_execute,
    [YS-XS, YF-XF] = MOVE,
    format('Computer greedily moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]), nl.

% choose_best_move(+GAMESTATE, +PLAYER, +VALIDMOVES, -MOVE)
% Chooses the best move using a greedy algorithm.
% If there are moves with the same value, it chooses randomly.
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
    random_choice(MOVE, MAXMOVES).
    % print_sorted_moves(SORTEDMOVESVALUE).  

% get_computer_answer(+GAMESTATE, +PLAYER, +BOTLEVEL)
% Chooses if the bot will play again after a jump.
% If BOTLEVEL is 1, it randomly choose.
% If BOTLEVEL is 2, it checks if the player can increase the value of the current board.
% If the value can increase chooses yes, otherwise no.
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

% choose_best_answer(+GAMESTATE, +PLAYER, -ANSWER)
% Checks if the player can increase the current board value with the next move.
% If the value can increase select yes, otherwise no.
choose_best_answer(GAMESTATE, PLAYER, ANSWER) :-
    value(GAMESTATE, PLAYER, CURRENTVALUE),
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    choose_best_move(GAMESTATE, PLAYER, VALIDMOVES, MOVE),
    execute_move(GAMESTATE, MOVE, NEWGAMESTATE),
    value(NEWGAMESTATE, PLAYER, FUTUREVALUE),
    (FUTUREVALUE > CURRENTVALUE ->
        ANSWER = 'yes'
    ;
        ANSWER = 'no'
    ).

