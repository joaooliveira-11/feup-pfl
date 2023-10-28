:- consult(game).

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


get_move(PLAYER, Move) :-
    repeat,
    means(PLAYER, NAME),
    format('~w pieces turn\n: ', [NAME]),
    write('Enter the coordinates of the piece to move (Y-X): '), nl,
    read(START),
    (valid_coordinates(START) ->
        true
    ; 
        write('Invalid coordinates. Please enter valid coordinates (Y-X) of the piece to move.'), nl,
        fail
    ),

    write('Enter the new coordinates of the piece to move (Y-X): '), nl,
    read(END),
    (valid_coordinates(END) ->
        true
    ; 
        write('Invalid coordinates. Please enter new valid coordinates (Y-X) of the piece to move.'), nl,
        fail
    ),
    Move = [START, END], !.

valid_coordinates(Y-X):-
    boardsize(SIZE),
    X > 0, X =< SIZE,
    Y > 0, Y =< SIZE.

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
set_boardsize(2, Input) :-
    retract(boardsize(_)),
    assert(boardsize(Input)).

/*
set_playerside :-
*/