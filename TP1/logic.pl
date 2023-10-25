% move(+GameState, +From, +To, -NewGameState)
move(GAMESTATE, FROM, TO, NEWGAMESTATE) :-
    select_piece(GAMESTATE, FROM, PIECE),
    place_piece(GAMESTATE, TO, PIECE, NEWGAMESTATE).

% select_piece(+GameState, +Coords, -Piece)
% Selects the piece at the given coordinates.
select_piece(GAMESTATE, X-Y, PIECE) :-
    nth1(X, GAMESTATE, ROW),
    nth1(Y, ROW, PIECE).

% place_piece(+GameState, +Coords, +Piece, -NewGameState)
% Places the given piece at the specified coordinates.
place_piece(GAMESTATE, X-Y, PIECE, NEWGAMESTATE) :-
    select_row(GAMESTATE, X, ROW),
    replace_at(Y, ROW, PIECE, NEWROW),
    replace_at(X, GAMESTATE, NEWROW, NEWGAMESTATE).

% select_row(+GameState, +Row, -SelectedRow)
% Selects the specified row from the game state.
select_row(GAMESTATE, ROW, SELECTEDROW) :-
    nth1(ROW, GAMESTATE, SELECTEDROW).

% replace_at(+Index, +List, +Element, -NewList)
% Replaces the element at the specified index in the list.
replace_at(INDEX, LIST, ELEMENT, NEWLIST) :-
    nth1(INDEX, LIST, ELEMENT, NEWLIST).

