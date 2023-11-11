:- dynamic round/4.

% round(RoundNumber, DanceStyle, Minutes, [Dancer1-Dancer2 | DancerPairs])
% round/4 indica, para cada ronda, o estilo de dança, a sua duração, e os pares de dançarinos participantes.
round(1, waltz, 8, [eugene-fernanda]).
round(2, quickstep, 4, [asdrubal-bruna,cathy-dennis,eugene-fernanda]).
round(3, foxtrot, 6, [bruna-dennis,eugene-fernanda]).
round(4, samba, 4, [cathy-asdrubal,bruna-dennis,eugene-fernanda]).
round(5, rhumba, 5, [bruna-asdrubal,eugene-fernanda]).

% tempo(DanceStyle, Speed).
% tempo/2 indica a velocidade de cada estilo de dança.
tempo(waltz, slow).
tempo(quickstep, fast).
tempo(foxtrot, slow).
tempo(samba, fast).
tempo(rhumba, slow).


%! (1)

% style_round_number(?DanceStyle, ?RoundNumber)
style_round_number(DanceStyle, RoundNumber) :-
    round(RoundNumber, DanceStyle, _T, _Pairs).

%! (2)
% n_dancers(?RoundNumber, -NDancers)  
n_dancers(RoundNumber, NDancers)  :-
    round(RoundNumber, _D, _T, Pairs),
    length(Pairs, PairsPerRound),
    NDancers is PairsPerRound * 2.

%! (3)
% danced_in_round(?RoundNumber, ?Dancer)
danced_in_round(RoundNumber, Dancer) :-
    round(RoundNumber, _D, _T, Pairs),
    member(Dancer-_Dancer2, Pairs).
danced_in_round(RoundNumber, Dancer) :-
    round(RoundNumber, _D, _T, Pairs),
    member(_Dancer2-Dancer, Pairs).

% -------------------------------------------------- 

/*
danced_in_round(RoundNumber, Dancer) :-
    round(RoundNumber, _D, _T, Pairs),
    (member(Dancer-_Dancer2, Pairs); member(_Dancer2-Dancer, Pairs)).
*/


%! (4)

% n_rounds(-NRounds)
n_rounds(NRounds) :-
    round(NRounds, _D, _T, _Pairs),
    \+ (
        round(RoundNumber1, _D1, _T1, _Pairs1),
        RoundNumber1 > NRounds
    ).
% ---------------------------------------------------------

/*
n_rounds(NRounds) :-
    n_rounds(0, NRounds).

n_rounds(RoundAcc, NRounds) :-
    RoundAcc1 is RoundAcc + 1,
    round(RoundAcc1, _D, _T, _Pairs),
    n_rounds(RoundAcc1, NRounds), !.
n_rounds(NRounds, NRounds).
*/


%! (5)

% add_dancer_pair(+RoundNumber, +Dancer1, +Dancer2)
add_dancer_pair(RoundNumber, Dancer1, Dancer2) :-
    \+ danced_in_round(RoundNumber, Dancer1),
    \+ danced_in_round(RoundNumber, Dancer2),
    append(Pairs, [Dancer1-Dancer2], NewPairs),
    retract(round(RoundNumber, _D, _T, Pairs)),
    assert(round(RoundNumber, _D, _T, NewPairs)).

%! (6) 

% total_dance_time(+Dancer, -Time)

total_dance_time(Dancer, Time) :-
    total_dance_time(Dancer, 1, 0, Time).

total_dance_time(Dancer, CurrRound, Acc, Time) :-
    round(CurrRound, _D, T, _Pairs),
    danced_in_round(CurrRound, Dancer),
    Acc1 is Acc + T,
    CurrRound1 is CurrRound + 1,
    total_dance_time(Dancer, CurrRound1, Acc1, Time), !.
total_dance_time(Dancer, CurrRound, Acc, Time) :-
    round(CurrRound, _D, _T, _Pairs),
    CurrRound1 is CurrRound + 1,
    total_dance_time(Dancer, CurrRound1, Acc, Time), !.
total_dance_time(_Dancer, _CurrRound, Time, Time).


%! (7)

% print_program/0
print_program :-
    round(_RoundNumber, D, T, Pairs),
    length(Pairs, PairsLength),
    write(D), write(' ('), write(T), write(') '), write('- '), write(PairsLength), nl,
    fail.
print_program.


%! (8)

% dancer_n_dances(?Dancer, ?NDances)
dancer_n_dances(Dancer, NDances) :-
    bagof(
        RoundNumber,
        danced_in_round(RoundNumber, Dancer),
        DancerRounds
    ),
    length(DancerRounds, NDances).

%! (9)
:- use_module(library(lists)).

% most_tireless_dancer(-Dancer)
most_tireless_dancer(Dancer) :-
    setof(
        DancerAux-Time,
        ^(
            danced_in_round(_RoundNumber, DancerAux),
            total_dance_time(DancerAux, Time)
        ),
        DancersTime
    ),
    reverse(DancersTime, ReversedDancersTime),
    [Dancer-_| _T] = ReversedDancersTime.




predX([],0).
predX([X|Xs],N):-
    X =.. [_|T],
    length(T,2),
    predX(Xs,N1),
    N is N1 + 1.
predX([_|Xs],N):-
    predX(Xs,N).