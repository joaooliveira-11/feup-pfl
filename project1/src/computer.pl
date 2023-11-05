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
    sleep(3).

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

% minimax_mode(+MODE, -NEWMODE)
% Changes minimax algorithm mode.
minimax_mode(min, max).
minimax_mode(max, min).

% minimax_value(+MODE, +VALUES, -VALUE)
% Chooses the value according to Minimax mode
minimax_value(min, VALUES, VALUE):-
    last(VALUES, TEMPVALUE),
    VALUE is -TEMPVALUE.
minimax_value(max, VALUES, VALUE):- 
    last(VALUES, VALUE).

% choose_move(+GAMESTATE, +PLAYER, +BOTLEVEL, -MOVE)
% Selects a move from a list of valid moves.
% If BOTLEVEL is 1, is a random choice.
% If BOTLEVEL is 2, is a random choice if more than 1 exist or the only one, from the options with the best value.
choose_move(GAMESTATE, PLAYER, 1, MOVE) :-
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    random_choice(MOVE, VALIDMOVES),
    %wait_execute,
    auto_execute,
    MOVE = [YS-XS, YF-XF],
    format('Computer randomly moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]),nl.
choose_move(GAMESTATE, PLAYER, 2, MOVE) :-
    valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
    choose_best_move(GAMESTATE, PLAYER, VALIDMOVES, MOVE),
    %wait_execute,
    auto_execute,
    [YS-XS, YF-XF] = MOVE,
    format('Computer greedily moved from (~w-~w) to (~w-~w).', [YS, XS, YF, XF]), nl.

% choose_best_move(+GAMESTATE, +PLAYER, +VALIDMOVES, -MOVE)
% Chooses the best move using Minimax algorithm.
% To choose the best move, we try to maximize the following formula: (Value of my best move (VALUE1) - Value of the next player best move after my best move(VALUE2))
% If there are moves with the same value, it chooses randomly.
choose_best_move(GAMESTATE, PLAYER, VALIDMOVES, MOVE) :-
    change_turn(PLAYER, NEXTPLAYER),
    findall(
        VALUE-MOVE,
        (
            member(MOVE, VALIDMOVES),
            execute_move(GAMESTATE, MOVE, NEWGAMESTATE),
            value(NEWGAMESTATE, PLAYER, VALUE1),
            minimax_ai(NEWGAMESTATE, NEXTPLAYER, min, 1, VALUE2),
            VALUE is VALUE1 + VALUE2
        ),
        MOVESVALUE
    ),
    keysort(MOVESVALUE, SORTEDMOVESVALUE),
    last(SORTEDMOVESVALUE, MAX-_),
    findall(MOVES, member(MAX-MOVES, SORTEDMOVESVALUE), MAXMOVES),
    random_choice(MOVE, MAXMOVES).

% minimax_ai(+GAMESTATE, +PLAYER, +MODE, +DEPTH, -VALUE)
% When choosing the best move, we only consider the current play and the play of the next player
% For this reason, we use Minimax algorithm with depth 2
minimax_ai(_, _, _, 2, 0):- !.
minimax_ai(GAMESTATE, PLAYER, MODE, DEPTH, VALUE):-
	change_turn(PLAYER, NEXTPLAYER),
	minimax_mode(MODE, NEWMODE),
    NEWDEPTH is DEPTH + 1,
	valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
	findall(TEMPVALUE, 
            (member(MOVE, VALIDMOVES), 
            execute_move(GAMESTATE, MOVE, NEWGAMESTATE),
            value(NEWGAMESTATE, PLAYER, VALUE1),
            minimax_ai(NEWGAMESTATE, NEXTPLAYER, NEWMODE, NEWDEPTH, VALUE2), 
            TEMPVALUE is VALUE1 + VALUE2
            ), 
        VALUES),
    sort(VALUES, SORTEDVALUES),
    minimax_value(MODE, SORTEDVALUES, VALUE).

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

