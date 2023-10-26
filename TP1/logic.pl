% move(+GAMESTATE, +FROM, +TO, -NEWGAMESTATE)
move(GAMESTATE, FROM, TO, NEWGAMESTATE) :-
    get_piece(GAMESTATE, FROM, PIECE),
    add_piece(GAMESTATE, FROM, 0, TEMPSTATE),
    add_piece(TEMPSTATE, TO, PIECE, NEWGAMESTATE).

% get_piece(+GAMESTATE, +(X-Y), -PIECE)
get_piece(GAMESTATE, X-Y, PIECE) :-
    nth1(X, GAMESTATE, ROW),
    nth1(Y, ROW, PIECE).

% add_piece(+GAMESTATE, +(X-Y), +PIECE, -NEWGAMESTATE)
add_piece(GAMESTATE, X-Y, PIECE, NEWGAMESTATE) :-
    get_row(GAMESTATE, X, ROW),
    replace_piece(Y, ROW, PIECE, NEWROW),
    replace_row(X, GAMESTATE, NEWROW, NEWGAMESTATE).

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

horizontal_left_pieces(_ ,_ ,_ , LEFTPIECES, LEFTPIECES).
horizontal_left_pieces(GAMESTATE, XS-YS, PIECE, ACC, LEFTPIECES) :-
    X1 is XS -1,
    X1 >= 1,
    get_piece(GAMESTATE, X1-YS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    horizontal_left_pieces(GAMESTATE, X1-YS, PIECE, ACC1, LEFTPIECES).

horizontal_right_pieces(_ ,_ ,_ , RIGHTPIECES, RIGHTPIECES).
horizontal_right_pieces(GAMESTATE, XS-YS, PIECE, ACC, RIGHTPIECES) :-
    X1 is XS + 1,
    boardsize(SIZE),
    X1 <= SIZE,
    get_piece(GAMESTATE, X1-YS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    horizontal_right_pieces(GAMESTATE, X1-YS, PIECE, ACC1, RIGHTPIECES).

vertical_top_pieces(_ ,_ ,_ , TOPPIECES, TOPPIECES).
vertical_top_pieces(GAMESTATE, XS-YS, PIECE, ACC, TOPPIECES) :-
    Y1 is YS -1,
    Y1 >= 1,
    get_piece(GAMESTATE, X1-YS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    vertical_top_pieces(GAMESTATE, X-Y1, PIECE, ACC1, TOPPIECES).

vertical_bottom_pieces(_ ,_ ,_ , BOTTOMPIECES, BOTTOMPIECES).
vertical_bottom_pieces(GAMESTATE, XS-YS, PIECE, ACC, BOTTOMPIECES) :-
    Y1 is YS + 1,
    boardsize(SIZE),
    Y1 <= SIZE,
    get_piece(GAMESTATE, X1-YS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    vertical_bottom_pieces(GAMESTATE, X-Y1, PIECE, ACC1, BOTTOMPIECES).

horizontal_length(GAMESTATE, XS-YS, PIECE, LENGTH) :-
    horizontal_left_pieces(GAMESTATE, XS-YS, PIECE, 0, LEFTPIECES),
    horizontal_right_pieces(GAMESTATE, XS-YS, PIECE, 0, RIGHTPIECES),
    LENGTH is LEFTPIECES + RIGHTPIECES + 1.

vertical_length(GAMESTATE, XS-YS, PIECE, LENGTH) :-
    vertical_top_pieces(GAMESTATE, XS-YS, PIECE, 0, TOPPIECES),
    vertical_bottom_pieces(GAMESTATE, XS-YS, PIECE, 0, BOTTOMPIECES),
    LENGTH is TOPPIECES + BOTTOMPIECES + 1.

get_direction(XS-YS, XF-YF, VERTICAL) :-
    XS = XF, YS \= YF, !.

get_direction(XS-YS, XF-YF, HORIZONTAL) :-
    XS \= XF, YS = YF, !.

get_direction(XS-YS, XF-YF, DIAGONAL) :- 
    XS \= XF, YS \= YF, !.

get_direction(XS-YS, XF-YF, INVALID) :- 
    XS = XF, YS = YF, !.
