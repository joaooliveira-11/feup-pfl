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