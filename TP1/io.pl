:- consult(game).
:- consult(logic).
:- use_module(library(random)).
:- use_module(library(system)).

read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

get_menuinput(LOWERBOUND, UPPERBOUND, INPUT) :-
    repeat,
    format('Please choose an option between ~w and ~w:', [LOWERBOUND, UPPERBOUND]),
    flush_output,
    read_number(OPTION),
    (
        between(LOWERBOUND, UPPERBOUND, OPTION), !, INPUT = OPTION
    ;
        format('Invalid option. ~w is not in the range ~w to ~w.\n', [OPTION, LOWERBOUND, UPPERBOUND]),
        fail
    ).


random_member(Elem, List) :-
    length(List, Length),
    random(0, Length, Index),
    nth0(Index, List, Elem).

get_move(GAMESTATE, MOVE) :-
    [_,_, PLAYER, GAMEMODE] = GAMESTATE,
    valid_moves(GAMESTATE, VALIDMOVES),
    (GAMEMODE = c/c ->
        print_player_turn(PLAYER),
        sleep(3),  
        valid_moves(GAMESTATE, VALIDMOVES),
        random_member(MOVE, VALIDMOVES),
        MOVE = [YS-XS, YF-XF],
        format('computer moved from (~w-~w) to (~w-~w)', [YS, XS, YF, XF]),nl
    ;
    (repeat,
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
    MOVE = [START, END], !)
    ).

ask_to_play_again(GAMESTATE) :-
    [_,_, PLAYER, GAMEMODE] = GAMESTATE,
    (GAMEMODE = c/c ->
        write('Since you made a jump and the jumped piece can move again, you are allowed to play again\n'),
        write('Do you want to play again (yes or no)?\n'),
        random_member(ANSWER, ['yes', 'no']),
        format('computer chose: ~w\n', [ANSWER]),
        (
        ANSWER = 'yes' ->
            play_game(GAMESTATE)
        ;
        ANSWER = 'no' ->
            [BOARD, SIZE, PLAYER, GAMEMODE] = GAMESTATE,
            allow_single_steps(PLAYER),
            clear_blocked_positions(PLAYER),
            remove_continousmove_piece(PLAYER),
            change_turn(PLAYER, NEXTPLAYER),
            NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE],    
            play_game(NEWGAMESTATE)
        )
    ;
    (repeat,
    write('Since you made a jump and the jumped piece can move again, you are allowed to play again\n'),
    write('Do you want to play again (yes or no)?\n'),
    catch(read(ANSWER), _, (write('Invalid input. Please enter yes or no.\n'),fail)),
    (
        ANSWER = 'yes' ->
            play_game(GAMESTATE)
        ;
        ANSWER = 'no' ->
            [BOARD, SIZE, PLAYER, GAMEMODE] = GAMESTATE,
            allow_single_steps(PLAYER),
            clear_blocked_positions(PLAYER),
            remove_continousmove_piece(PLAYER),
            change_turn(PLAYER, NEXTPLAYER),
            NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE],    
            play_game(NEWGAMESTATE)
        ;
        write('Invalid input. Please enter yes or no.\n'),
        ask_to_play_again(GAMESTATE)
    )
    )
    ).

set_gamemode(1) :-
    retract(gamemode(_)),
    assert(gamemode(h/h)).
set_gamemode(2) :-
    retract(gamemode(_)),
    assert(gamemode(h/c)).
set_gamemode(3) :-
    retract(gamemode(_)),
    assert(gamemode(c/c)).

set_boardsize(0, _) :-
    main_menu.
set_boardsize(1, _) :-
    true.
set_boardsize(2, INPUT) :-
    retract(boardsize(_)),
    assert(boardsize(INPUT)).

/*
set_playerside :-
*/