% dish(Name, Price, IngredientGrams).
dish(pizza, 2200, [cheese-300, tomato-350]).
dish(ratatouille, 2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread, 1600, [cheese-50, garlic-200]).
:- dynamic ingredient/2.
% ingredient(Name, CostPerGram).
ingredient(cheese, 4).
ingredient(tomato, 2).
ingredient(eggplant, 7).
ingredient(garlic, 6).


%! (1)

% count_ingredients(?Dish, ?NumIngredients)
count_ingredients(Dish, NumIngredients) :-
    dish(Dish, _Price, IngredientGrams),
    length(IngredientGrams, NumIngredients).


%! (2)

% ingredient_amount_cost(?Ingredient, +Grams, ?TotalCost)
ingredient_amount_cost(Ingredient, Grams, TotalCost) :-
    ingredient(Ingredient, CostPerGram),
    TotalCost is CostPerGram * Grams.


%! (3)

% dish_profit(?Dish, ?Profit)
dish_profit(Dish, Profit) :-
    dish(Dish, SellPrice, IngredientGrams),
    price_to_make(IngredientGrams, 0, PriceToMake),
    Profit is SellPrice - PriceToMake.

price_to_make([], PriceToMake, PriceToMake).
price_to_make([CurrIng-Grams|T], Acc, PriceToMake) :-
    ingredient(CurrIng, CostPerGram),
    Acc1 is Acc + (CostPerGram * Grams),
    price_to_make(T, Acc1, PriceToMake).


%! (4)

% update_unit_cost(+Ingredient, +NewUnitCost)
update_unit_cost(Ingredient, NewUnitCost) :-
    retractall(ingredient(Ingredient, _)),
    assert(ingredient(Ingredient, NewUnitCost)).

% -------------------------------------------

/*
update_unit_cost(Ingredient, NewUnitCost) :- 
    retract(ingredient(Ingredient, _)),
    assert(ingredient(Ingredient, NewUnitCost)).
update_unit_cost(Ingredient, NewUnitCost) :- 
    assert(ingredient(Ingredient, NewUnitCost)).
*/

%! (5)

% most_expensive_dish(?Dish, ?Price)
most_expensive_dish(Dish, Price) :-
    dish(Dish, Price, _),
    \+ (
        dish(_Dish1, Price1, _),
        Price1 > Price
    ).

%! (6)

% consume_ingredient(+IngredientStocks, +Ingredient, +Grams, ?NewIngredientStocks)
consume_ingredient(IngredientStocks, Ingredient, Grams, NewIngredientStocks) :-
    append(Prev,[Ingredient-IngGrams|T],IngredientStocks),
    NewIngGrams is IngGrams - Grams,
    NewIngGrams > 0,
    append(Prev, [Ingredient-NewIngGrams], TempList),
    append(TempList, T, NewIngredientStocks).


%! (7)

% count_dishes_with_ingredient(+Ingredient, ?N)
count_dishes_with_ingredient(Ingredient, N) :-
    count_dishes(Ingredient, [], 0, N).

count_dishes(Ingredient, ListAcc, Acc, N) :-
    dish(Dish, _P, IngredientGrams),
    member(Ingredient-_Grams, IngredientGrams),
    \+ member(Dish, ListAcc),
    append(ListAcc, [Dish], ListAcc1),
    Acc1 is Acc + 1,
    count_dishes(Ingredient, ListAcc1, Acc1, N), !.
count_dishes(_Ingredient, _ListAcc, N, N).


%! (8)

% list_dishes(?DishIngredients)
list_dishes(DishIngredients) :-
    findall(
        Dish-Ingredients,
        (
           dish(Dish, _Price, IngredientGrams), 
           findall(
                Ingredient,
                member(Ingredient-_, IngredientGrams),
                Ingredients
           )
        ),
        DishIngredients
    ).

/*
:- use_module(library(lists)).

list_dishes(DishIngredients) :-
    findall(
        Dish-Ingredients,
        (
            dish(Dish, _Price, IngredientGrams),
            maplist(remove_grams, IngredientGrams, Ingredients)
        ),
        DishIngredients
    ).

    remove_grams(Ingredient-_, Ingredient).
*/


%! (9)

% most_lucrative_dishes(?Dishes) :-
most_lucrative_dishes(Dishes) :-
    setof(
        Profit-Dish,
        dish_profit(Dish, Profit),
        DishesAux
    ),
    remove_profit(DishesAux, [], Dishes).

remove_profit([], Dishes, Dishes).
remove_profit([_Profit-Dish|T], Acc, Dishes) :-
    append([Dish], Acc, Acc1),
    remove_profit(T, Acc1, Dishes).
*


%G1
edge(g1, br, o).
edge(g1, br, ni).
edge(g1, o, ni).
edge(g1, o, c).
edge(g1, o, h).
edge(g1, h, c).
edge(g1, h, n).
edge(g1, n, he).
edge(g1, c, he).
% G2
edge(g2, br, h).
edge(g2, br, ni).
edge(g2, h, ni).
edge(g2, h, o).
edge(g2, h, c).
edge(g2, o, c).
edge(g2, o, n).
edge(g2, n, he).
edge(g2, c, he).
edge(g2, cl, he).


% common_edges(+G1, +G2, ?L)
common_edges(G1, G2, L) :-
    findall(
        Orig-Dest,
        edge(G1, Orig, Dest),
        G1Edges
    ),
    findall(
        Orig-Dest,
        edge(G2, Orig, Dest),
        G2Edges
    ),
    extract_common_edges(G1Edges, G2Edges, [], L).


extract_common_edges([], _G2Edges, L, L).
extract_common_edges([Orig-Dest|T], G2Edges, Acc, L) :-
    member(Orig-Dest, G2Edges),
    append(Acc, [Orig-Dest], Acc1),
    extract_common_edges(T, G2Edges, Acc1, L), !.
extract_common_edges([_|T], G2Edges, Acc, L) :-
    extract_common_edges(T, G2Edges, Acc, L).

