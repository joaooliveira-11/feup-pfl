:-use_module(library(lists)).

% 2. Recursion over Lists

list_size(List, Size):-
    list_aux(List, 0, Size).

list_aux([], Size, Size).
list_aux([_|T], Acc, Size) :-
    Acc1 is Acc + 1,
    list_aux(T, Acc1, Size).


list_sum(List, Sum) :-
    list_sum_aux(List, 0, Sum).

list_sum_aux([], Sum, Sum).
list_sum_aux([H|T], Acc, Sum) :-
    Acc1 is Acc + H,
    list_sum_aux(T, Acc1, Sum).


list_prod(List, Prod) :-
    list_prod_aux(List, 1, Prod).

list_prod_aux([], Prod, Prod).
list_prod_aux([H|T], Acc, Prod) :-
    Acc1 is Acc * H,
    list_prod_aux(T, Acc1, Prod).


inner_product(List1, List2, Result) :-
    inner_product_aux(List1, List2, 0, Result).

inner_product_aux([], [], Result, Result).
inner_product_aux([H|T], [H1|T1], Acc, Result) :-
    Acc1 is Acc + (H*H1),
    inner_product_aux(T, T1, Acc1, Result).


count(Elem, List, N) :-
    count_aux(Elem, List, 0, N).

count_aux(_, [], N, N).
count_aux(Elem, [Elem|T], Acc, N) :-
    Acc1 is Acc + 1,
    count_aux(Elem, T, Acc1, N), !.
count_aux(Elem, [_|T], Acc, N) :-
    count_aux(Elem, T, Acc, N), !.


% 3. List Manipulation

invert(List1, List2) :-
    invert_aux(List1, [], List2).   

invert_aux([], List2, List2).
invert_aux([H|T], Acc, List2) :-
    Acc1 = [H|Acc],
    invert_aux(T, Acc1, List2). 


del_one(Elem, [Elem|T], T).
del_one(Elem, [H|T], [H|Result]) :-
    del_one(Elem, T, Result), !.


% 4. Append, The Powerful  

list_append([], Result, Result).
list_append([H|T], L2, [H|Result]) :-
    list_append(T, L2, Result).


list_member(Elem, List) :-
    append(_Prev, [Elem|_T], List).


list_last(List, Last) :-
    append(_Prev, [Last|[]], List).

/*
list_last(List, Last) :-
    append(_Prev, [Last], List).
*/

list_nth(N, List, Elem) :-
    length(List, Length),
    length(List1, N),
    N < Length,
    append(List1, [Elem|_T], List).


list_append_aux(ListOfLists, Result) :-
    list_append_2(ListOfLists, [], Result).

list_append_2([], Result, Result).
list_append_2([H|T], Acc, Result) :-
    append(Acc, H, Acc1),
    list_append_2(T, Acc1, Result).


list_del(List, Elem, Res) :-
    append(_Prev, [Elem|T], List),
    append(_Prev, T, Res).


list_before(First, Second, List) :-
    append(Prev, [Second|T], List),
    !,
    append(Prev1, [First|T1], Prev).


list_replace_one(X, Y, List1, List2) :-
    append(Prev, [X|T], List1),
    append(Prev, [Y|T], List2).

list_repeated(X, List) :-
    append(_, [X|T],List),
    append(_, [X|_], T).


% 5. Lists of Numbers

list_to(N, List):-
    list_to_aux(N, [], List).

list_to_aux(0, List, List).
list_to_aux(N, Acc, List) :-
    N > 0,
    N1 is N - 1,
    Acc1 = [N | Acc],
    list_to_aux(N1, Acc1, List).

list_from_to(Inf, Sup, List) :-
    Infaux is Inf -1,
    list_from_to_aux(Infaux, Sup, [], List).

list_from_to_aux(Sup, Sup, List, List).
list_from_to_aux(Inf, Sup, Acc, List) :-
    Sup > Inf,
    Acc1 = [Sup| Acc],
    Sup1 is Sup -1,
    list_from_to_aux(Inf, Sup1, Acc1, List).


list_from_to_step(Inf, Sup, Step, List) :-
    Infaux is Inf -Step,
    list_from_to_step_aux(Infaux, Sup, Step, [], List).

list_from_to_step_aux(Sup, Sup, _, List, List).
list_from_to_step_aux(Inf, Sup, Step, Acc, List) :-
    Sup > Inf,
    Acc1 = [Sup| Acc],
    Sup1 is Sup - Step,
    list_from_to_step_aux(Inf, Sup1, Step, Acc1, List).


% 7. List Sorting

is_ordered([H|T]):-
    is_ordered_aux(H, T).

is_ordered_aux(_, []).
is_ordered_aux(Prev, [H|T]) :-
    Prev =< H,
    is_ordered_aux(H, T).

    