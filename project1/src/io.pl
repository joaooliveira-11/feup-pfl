:- consult(game).
:- consult(logic).
:- consult(computer).

% set_gamemode(+OPTION)
% Sets the gamemode
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

% set_boardsize(+OPTION, +INPUT)
% Sets the board size.
set_boardsize(0, _) :-
    main_menu.
set_boardsize(1, _) :-
    true.
set_boardsize(2, INPUT) :-
    retract(boardsize(_)),
    assert(boardsize(INPUT)).

% set_playerside(+OPTION)
% Since we tell the player turn in the game, we cant really set a player side if we dont use additional data such as name.
set_playerside(0) :-
    main_menu.
set_playerside(1) :-
    true.
set_playerside(2) :-
    true.

% set_bot_level(+OPTION)
% Sets the bot level.
set_bot_level(0) :-
    main_menu.
set_bot_level(1) :-
    true.
set_bot_level(2) :-
    retract(bot_level(_)),
    assert(bot_level(2)).

% get_menuinput(+LOWERBOUND,+UPPERBOUND, +INPUT)
% Validates the user input.
get_menuinput(LOWERBOUND, UPPERBOUND, INPUT) :-
    repeat,
    format('Please choose an option between ~w and ~w:', [LOWERBOUND, UPPERBOUND]),
    (catch(read(OPTION), _, fail), between(LOWERBOUND, UPPERBOUND, OPTION) ->
        INPUT = OPTION
    ;
        format('Invalid option. Option is not in the range ~w to ~w.\n', [LOWERBOUND, UPPERBOUND]),
        fail
    ), !.

% get_human_move(GAMESTATE, MOVE)
% Asks the player, if human, for a move.
% Validates if the coordinates are inside the board.
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

% get_move(+GAMESTATE, +GAMEMODE, +MOVE)
% Gets a move from the player
% Takes into consideration if the current player is a human or bot.
get_move(GAMESTATE, h/h, MOVE) :-
    get_human_move(GAMESTATE, MOVE).
get_move(GAMESTATE, h/c, MOVE):-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    (PLAYER = 'W' ->
        get_human_move(GAMESTATE, MOVE)
    ;
        print_player_turn(PLAYER), 
        choose_move(GAMESTATE, PLAYER, BOTLEVEL, MOVE)
    ).
get_move(GAMESTATE, c/h, MOVE):-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    (PLAYER = 'B' ->
        get_human_move(GAMESTATE, MOVE)
    ;
        print_player_turn(PLAYER), 
        choose_move(GAMESTATE, PLAYER, BOTLEVEL, MOVE)
    ).
get_move(GAMESTATE, c/c, MOVE):-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    print_player_turn(PLAYER),
    choose_move(GAMESTATE, PLAYER, BOTLEVEL, MOVE). 

% get_human_answer(+GAMESTATE)
% Asks the player, if human, if he wants to play again after a jump.
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
        get_human_answer(GAMESTATE)
    ).


% ask_to_play_again(+GAMESTATE, +GAMEMODE)
% Asks the player if he wants to play again.
% Takes into consideration if the current player is a human or bot.
ask_to_play_again(GAMESTATE, h/h) :-
    get_human_answer(GAMESTATE).
ask_to_play_again(GAMESTATE, h/c) :-
    [_,_, PLAYER, _,BOTLEVEL] = GAMESTATE,
    (PLAYER = 'W' ->
        get_human_answer(GAMESTATE)
    ;
        get_computer_answer(GAMESTATE, PLAYER, BOTLEVEL)
    ).
ask_to_play_again(GAMESTATE, c/h) :-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    (PLAYER = 'B' ->
        get_human_answer(GAMESTATE)
    ;
        get_computer_answer(GAMESTATE, PLAYER, BOTLEVEL)
    ).
ask_to_play_again(GAMESTATE, c/c) :-
    [_,_, PLAYER, _, BOTLEVEL] = GAMESTATE,
    get_computer_answer(GAMESTATE, PLAYER, BOTLEVEL).