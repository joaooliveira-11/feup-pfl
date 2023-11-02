:- use_module(library(random)).
:- use_module(library(system)).
:- consult(logic).
:- consult(game).

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

get_computer_answer(GAMESTATE, 1) :-
    write('Since you made a jump and the jumped piece can move again, you are allowed to play again.\n'),
    write('Do you want to play again (yes or no)?\n'),
    random_choice(ANSWER, ['yes', 'no']),
    format('computer chose: ~w.\n', [ANSWER]),
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