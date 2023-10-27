% move(+GAMESTATE, +FROM, +TO, -NEWGAMESTATE)
move(GAMESTATE, FROM, TO, NEWGAMESTATE) :-
    get_piece(GAMESTATE, FROM, PIECE),
    add_piece(GAMESTATE, FROM, 0, TEMPSTATE),
    add_piece(TEMPSTATE, TO, PIECE, NEWGAMESTATE).

% get_piece(+GAMESTATE, +(Y-X), -PIECE)
get_piece(GAMESTATE, Y-X, PIECE) :-
    nth1(Y, GAMESTATE, ROW),
    nth1(X, ROW, PIECE).

% add_piece(+GAMESTATE, +(Y-X), +PIECE, -NEWGAMESTATE)
add_piece(GAMESTATE, Y-X, PIECE, NEWGAMESTATE) :-
    get_row(GAMESTATE, Y, ROW),
    replace_piece(X, ROW, PIECE, NEWROW),
    replace_row(Y, GAMESTATE, NEWROW, NEWGAMESTATE).

% get_row(+GAMESTATE, +INDEX, -SELECTEDROW)
get_row(GAMESTATE, INDEX, SELECTEDROW) :-
    nth1(INDEX, GAMESTATE, SELECTEDROW).

% replace_piece(+INDEX, +ROW, +PIECE, -NEWROW)
replace_piece(INDEX, ROW, PIECE, NEWROW) :-
    nth1(INDEX, ROW, _, TEMPROW),  % Discard the old element at Index
    nth1(INDEX, NEWROW, PIECE, TEMPROW).

% get_row(+INDEX, +BOARD, +PIECE, -NEWBOARD)
replace_row(INDEX, BOARD, PIECE, NEWBOARD) :-
    nth1(INDEX, BOARD, _, TEMPBOARD),  % Discard the old element at Index
    nth1(INDEX, NEWBOARD, PIECE, TEMPBOARD).

get_direction(YS-XS, YF-XF, vertical) :-
    XS = XF, YS \= YF, !.
get_direction(YS-XS, YF-XF, horizontal) :-
    XS \= XF, YS = YF, !.
get_direction(YS-XS, YF-XF, ldiagonal) :- 
    (XF > XS, YF > YS) ; (XF < XS, YF < YS),
    !.
get_direction(YS-XS, YF-XF, rdiagonal) :- 
    (XF < XS, YF > YS) ; (XF > XS, YF < YS),
    !.
get_direction(YS-XS, YF-XF, invalid) :- 
    XS = XF, YS = YF, !.
