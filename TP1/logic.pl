move(GAMESTATE, FROM, TO, NEWGAMESTATE) :-
    get_piece(GAMESTATE, FROM, PIECE),
    add_piece(GAMESTATE, FROM, 0, TEMPSTATE),
    display_game(TEMPSTATE),
    add_piece(TEMPSTATE, TO, PIECE, NEWGAMESTATE).

% get_piece(+GameState, +Coords, -Piece)
get_piece(GAMESTATE, X-Y, PIECE) :-
    nth1(X, GAMESTATE, ROW),
    nth1(Y, ROW, PIECE).

% add_piece(+GameState, +Coords, +Piece, -NewGameState)
add_piece(GAMESTATE, X-Y, PIECE, NEWGAMESTATE) :-
    get_row(GAMESTATE, X, ROW),
    replace_at(Y, ROW, PIECE, NEWROW),
    replace_at(X, GAMESTATE, NEWROW, NEWGAMESTATE).

% get_row(+GameState, +INDEX, -SelectedRow)
get_row(GAMESTATE, INDEX, SELECTEDROW) :-
    nth1(INDEX, GAMESTATE, SELECTEDROW).

% replace_at(+Index, +List, +Element, -NewList)
replace_at(INDEX, LIST, ELEMENT, NEWLIST) :-
    nth1(INDEX, LIST, ELEMENT, NEWLIST).

