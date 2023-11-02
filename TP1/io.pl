:- consult(game).
:- consult(logic).
:- consult(computer).

set_gamemode(1) :-
    retract(gamemode(_)),
    assert(gamemode(h/h)).
set_gamemode(2) :-
    retract(gamemode(_)),
    assert(gamemode(h/c)).
set_gamemode(3) :-
    retract(gamemode(_)),
    assert(gamemode(c/h)).
set_gamemode(4) :-
    retract(gamemode(_)),
    assert(gamemode(c/c)).

set_boardsize(0, _) :-
    main_menu.
set_boardsize(1, _) :-
    true.
set_boardsize(2, INPUT) :-
    retract(boardsize(_)),
    assert(boardsize(INPUT)).

set_playerside(0) :-
    main_menu.
set_playerside(1) :-
    true.
set_playerside(2) :-
    true.

set_bot_level(0) :-
    main_menu.
set_bot_level(1) :-
    true.
set_bot_level(2) :-
    retract(bot_level(_)),
    assert(bot_level(2)).

get_menuinput(LOWERBOUND, UPPERBOUND, INPUT) :-
    repeat,
    format('Please choose an option between ~w and ~w:', [LOWERBOUND, UPPERBOUND]),
    (catch(read(OPTION), _, fail), between(LOWERBOUND, UPPERBOUND, OPTION) ->
        INPUT = OPTION
    ;
        format('Invalid option. Option is not in the range ~w to ~w.\n', [LOWERBOUND, UPPERBOUND]),
        fail
    ), !.

get_human_move(GAMESTATE, MOVE) :-
    repeat,
    [_,_, PLAYER, _, _] = GAMESTATE,
    print_player_turn(PLAYER),
    write('Enter the coordinates of the piece to move (Y-X): '), nl,
    (catch(read(START), _, fail), valid_coordinates(START) ->
        true
    ; 
        write('Invalid coordinates. Please enter valid coordinates (Y-X) of the piece to move.'), nl,
        fail
    ),

    write('Enter the new coordinates of the piece to move (Y-X): '), nl,
    (catch(read(END), _, fail), valid_coordinates(END) ->
        true
    ; 
        write('Invalid coordinates. Please enter new valid coordinates (Y-X) of the piece to move.'), nl,
        fail
    ),
    MOVE = [START, END], !.

get_move(GAMESTATE, h/h, MOVE) :-
    get_human_move(GAMESTATE, MOVE).

get_move(GAMESTATE, h/c, MOVE):-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    (PLAYER = 'W' ->
        get_human_move(GAMESTATE, MOVE)
    ;
        print_player_turn(PLAYER),
        sleep(3), 
        choose_move(GAMESTATE, BOTLEVEL, MOVE)
    ).

get_move(GAMESTATE, c/h, MOVE):-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    (PLAYER = 'B' ->
        get_human_move(GAMESTATE, MOVE)
    ;
        print_player_turn(PLAYER),
        sleep(3), 
        choose_move(GAMESTATE, BOTLEVEL, MOVE)
    ).

get_move(GAMESTATE, c/c, MOVE):-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    print_player_turn(PLAYER),
    sleep(3), 
    choose_move(GAMESTATE, BOTLEVEL, MOVE). 

get_human_answer(GAMESTATE) :-
    [_,_, PLAYER, GAMEMODE, _] = GAMESTATE,
    repeat,
    write('Since you made a jump and the jumped piece can move again, you are allowed to play again.\n'),
    write('Do you want to play again (yes or no)?\n'),
    catch(read(ANSWER), _, (write('Invalid input. Please enter yes or no.\n'),fail)),
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
        ;
        write('Invalid input. Please enter yes or no.\n'),
        ask_to_play_again(GAMESTATE)
    ).

ask_to_play_again(GAMESTATE, h/h) :-
    get_human_answer(GAMESTATE).

ask_to_play_again(GAMESTATE, h/c) :-
    [_,_, PLAYER, _,BOTLEVEL] = GAMESTATE,
    (PLAYER = 'W' ->
        get_human_answer(GAMESTATE)
    ;
        get_computer_answer(GAMESTATE, BOTLEVEL)
    ).

ask_to_play_again(GAMESTATE, c/h) :-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    (PLAYER = 'B' ->
        get_human_answer(GAMESTATE)
    ;
        get_computer_answer(GAMESTATE, BOTLEVEL)
    ).

ask_to_play_again(GAMESTATE, c/c) :-
    [_,_, _, _, BOTLEVEL] = GAMESTATE,
    get_computer_answer(GAMESTATE, BOTLEVEL).