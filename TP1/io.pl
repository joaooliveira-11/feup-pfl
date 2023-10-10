:- consult(game).

read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

ask_menuOption(Lowerbound, Upperbound, Option) :- 
    format('Please choose an option between ~w and ~w:', [Lowerbound, Upperbound]),
    flush_output,
    read_number(Input),
    (Input >= Lowerbound, Input =< Upperbound ->
        Option = Input
    ;
        format('Invalid option. ~w is not in the range ~w to ~w.\n', [Input, Lowerbound, Upperbound]),
        ask_menuOption(Lowerbound, Upperbound, Option)
    ).

ask_boardsize(Lowerbound, Upperbound):-
    format('Please choose an option between ~w and ~w:', [Lowerbound, Upperbound]),
    flush_output,
    read_number(Size),
    (Size >= Lowerbound, Size =< Upperbound ->
        retract(boardsize(_)),
        assert(boardsize(Size))
    ;
        format('Invalid option. ~w is not in the range ~w to ~w.\n', [Size, Lowerbound, Upperbound]),
        ask_boardsize(Lowerbound, Upperbound)
    ).

input_handler(Lowerbound, Upperbound, Option) :-
    ask_menuOption(Lowerbound, Upperbound, Option).

handle_mainMenu(Option) :-
    (Option == 1 ->
        retract(gamemode(_)),
        assert(gamemode(normal)),
        boardsize_menu
    ;
    Option == 2 ->
        retract(gamemode(_)),
        assert(gamemode(normal2)),
        boardsize_menu
    ;
    Option == 3 ->
        retract(gamemode(_)),
        assert(gamemode(normal3)),
        boardsize_menu
    ).

handle_boardsizeMenu(Option) :-
    (Option == 1 ->
        play_game % por agora já que não resolvemos a situação do player
    ;
    Option == 2 ->
        ask_boardsize(9,26),
        play_game % por agora já que não resolvemos a situação do player
    ;
    Option == 0 ->
        main_menu
    ).


