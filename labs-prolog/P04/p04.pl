:- use_module(library(between)).

s(1).
s(2):- !.
s(3).


data(one).
data(two).
data(three).

cut_test_a(X):- data(X).
cut_test_a('five').

cut_test_b(X):- data(X), !.
cut_test_b('five').

cut_test_c(X, Y):- data(X), !, data(Y).
cut_test_c('five', 'five').


adult(joao).

immature(X):- adult(X), fail.
immature(_X).

max_aux(A, B, C, Max) :-
    A >= B,
    A >= c,
    Max = A, !.
max_aux(A, B, C, Max) :-
    B >= A,
    B >= C,
    Max = B, !.
max_aux(A, B, C, Max) :-
    Max = C.


% 5a)

print_n(N, S) :-
    between(1, N, _Number),
    write(S),
    nl,
    fail.

print_n_2(0, _S) :- !.
print_n_2(N, S) :-
    write(S),
    nl,
    N1 is N - 1,
    print_n_2(N1, S).

% 5b)

print_spaces(0) :- !.
print_spaces(Padding) :-
    write(' '),
    Padding1 is Padding - 1,
    print_spaces(Padding1).

write_text([]).
write_text([H|T]) :-
    put_code(H),
    write_text(T).

print_text(Text, Symbol, Padding) :-
    write(Symbol),
    print_spaces(Padding),
    write_text(Text),
    print_spaces(Padding),
    write(Symbol).

% 5c)
print_symbol(_Symbol, 0) :- !.
print_symbol(Symbol, N) :-
    write(Symbol),
    N1 is N-1,
    print_symbol(Symbol, N1).

print_banner(Text, Symbol, Padding) :-
    length(Text, Length),
    print_symbol(Symbol, Length + Padding * 2 + 2), nl,
    print_symbol(Symbol, 1), print_spaces(Padding * 2 + Length), print_symbol(Symbol, 1), nl,
    print_symbol(Symbol, 1), print_spaces(Padding), write_text(Text), print_spaces(Padding), print_symbol(Symbol, 1), nl,
    print_symbol(Symbol, 1), print_spaces(Padding * 2 + Length ), print_symbol(Symbol, 1), nl,
    print_symbol(Symbol, Length + Padding * 2 + 2), nl.


% 5d)

read_number(X) :-
    read_number_aux(X, 0, false).

read_number_aux(X, Acc, _) :-
    get_code(C),
    C >= 48, C =< 57,
    Acc1 is  10 * Acc + (C-48),
    read_number_aux(X, Acc1, true).
read_number_aux(Acc, Acc, true).

/*
read_number_aux(X, Acc, _) :-
    peek_code(C),
    C \= 10,
    get_code(C),
    C >= 48, C =< 57,
    Acc1 is  10 * Acc + (C-48),
    read_number_aux(X, Acc1, true).
read_number_aux(Acc, Acc, true) :-
    get_code(_X).

*/
% 5e)

read_until_between(Min, Max, Value) :-
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.



% 6. List Printing

% 6a)

print_full_list(L) :-
    write('['), print_full_list_aux(L).

print_full_list_aux([H|[]]) :-
    write(H), write(']'), !.
print_full_list_aux([H|T]) :-
    write(H), write(','), write(' '),
    print_full_list_aux(T). 

% 6b)

print_list(L) :-
    length(L, Length), print_list_aux(L, Length).

print_list_aux(L, _Length) :-
    _Length =< 11,
    print_full_list(L).
% print_list_aux(L, _Length) :-
    % não sei fazer a >= 12

% 6c)


print_matrix(Matrix) :-
    print_matrix_aux(Matrix).

print_matrix_aux([]).
print_matrix_aux([H|T]) :-
    print_full_list(H),
    nl,
    print_matrix_aux(T).


