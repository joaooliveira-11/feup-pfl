% execute_move(+GAMESTATE, +FROM, +TO, -NEWGAMESTATE)
% Executes a move by replacing the Piece in the position 'TO' by the piece at the position 'FROM'
execute_move(GAMESTATE, FROM, TO, NEWGAMESTATE) :-
    get_piece(GAMESTATE, FROM, PIECE),
    add_piece(GAMESTATE, FROM, 0, TEMPSTATE),
    add_piece(TEMPSTATE, TO, PIECE, NEWGAMESTATE).

% get_piece(+GAMESTATE, +(Y-X), -PIECE)
% Gets the piece in the specified position
get_piece(GAMESTATE, Y-X, PIECE) :-
    nth1(Y, GAMESTATE, ROW),
    nth1(X, ROW, PIECE).

% add_piece(+GAMESTATE, +(Y-X), +PIECE, -NEWGAMESTATE)
% Gets the board row that matches the y coordinates
% Replaces the collumn x of that row with the input piece
% Replaces the row with the modified piece into the board
add_piece(GAMESTATE, Y-X, PIECE, NEWGAMESTATE) :-
    get_row(GAMESTATE, Y, ROW),
    replace_piece(X, ROW, PIECE, NEWROW),
    replace_row(Y, GAMESTATE, NEWROW, NEWGAMESTATE).

% get_row(+GAMESTATE, +INDEX, -SELECTEDROW)
% Gets a board row in the specified index
get_row(GAMESTATE, INDEX, SELECTEDROW) :-
    nth1(INDEX, GAMESTATE, SELECTEDROW).

% replace_piece(+INDEX, +ROW, +PIECE, -NEWROW)
% Replaces the piece at the index input of the row input by the piece input
replace_piece(INDEX, ROW, PIECE, NEWROW) :-
    nth1(INDEX, ROW, _, TEMPROW),  % Discard the old element at Index
    nth1(INDEX, NEWROW, PIECE, TEMPROW).

% get_row(+INDEX, +BOARD, +PIECE, -NEWBOARD)
% Replaces the row in the index input of the board by the new row with the piece
replace_row(INDEX, BOARD, PIECE, NEWBOARD) :-
    nth1(INDEX, BOARD, _, TEMPBOARD),  % Discard the old element at Index
    nth1(INDEX, NEWBOARD, PIECE, TEMPBOARD).

% get_direction(+(YS-XS), +(YF-XF), -direction)
% Gets the move direction
get_direction(YS-XS, YF-XF, vertical) :-
    XS = XF, YS \= YF, !.
get_direction(YS-XS, YF-XF, horizontal) :-
    XS \= XF, YS = YF, !.
get_direction(YS-XS, YF-XF, ldiagonal) :- 
    ((XF > XS, YF > YS) ; (XF < XS, YF < YS)),
    DIFFY is YF - YS,
    DIFFX is XF - XS,
    DIFFY = DIFFX,
    !.
get_direction(YS-XS, YF-XF, rdiagonal) :- 
    ((XF < XS, YF > YS) ; (XF > XS, YF < YS)),
    DIFFY is YF - YS,
    DIFFX is XS - XF,
    DIFFY = DIFFX,
    !.
get_direction(_,_, invalid).

% horizontal_left_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -LEFTPIECES)
% Calculates the ammount of pieces that form a piece line starting at the left position of the original position
% Piece lines are lines formed with only one type of piece, in this case the player piece
horizontal_left_pieces(GAMESTATE, YS-XS, PIECE, ACC, LEFTPIECES) :-
    X1 is XS -1,
    X1 >= 1,
    get_piece(GAMESTATE, YS-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    horizontal_left_pieces(GAMESTATE, YS-X1, PIECE, ACC1, LEFTPIECES).
horizontal_left_pieces(_ ,_ ,_ , LEFTPIECES, LEFTPIECES).

% horizontal_right_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -RIGHTPIECES)
% Calculates the ammount of pieces that form a piece line starting at the right position of the original position
% Piece lines are lines formed with only one type of piece, in this case the player piece
horizontal_right_pieces(GAMESTATE, YS-XS, PIECE, ACC, RIGHTPIECES) :-
    X1 is XS + 1,
    boardsize(SIZE),
    X1 =< SIZE,
    get_piece(GAMESTATE, YS-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    horizontal_right_pieces(GAMESTATE, YS-X1, PIECE, ACC1, RIGHTPIECES).
horizontal_right_pieces(_ ,_ ,_ , RIGHTPIECES, RIGHTPIECES).

% vertical_top_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -TOPPIECES)
% Calculates the ammount of pieces that form a piece line starting at the above position of the original position
% Piece lines are lines formed with only one type of piece, in this case the player piece
vertical_top_pieces(GAMESTATE, YS-XS, PIECE, ACC, TOPPIECES) :-
    Y1 is YS - 1,
    Y1 >= 1,
    get_piece(GAMESTATE, Y1-XS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    vertical_top_pieces(GAMESTATE, Y1-XS, PIECE, ACC1, TOPPIECES).
vertical_top_pieces(_, _, _, TOPPIECES, TOPPIECES).

% vertical_bottom_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -BOTTOMPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position bellow of the original position
% Piece lines are lines formed with only one type of piece, in this case the player piece
vertical_bottom_pieces(GAMESTATE, YS-XS, PIECE, ACC, BOTTOMPIECES) :-
    Y1 is YS + 1,
    boardsize(SIZE),
    Y1 =< SIZE,
    get_piece(GAMESTATE, Y1-XS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    vertical_bottom_pieces(GAMESTATE, Y1-XS, PIECE, ACC1, BOTTOMPIECES).
vertical_bottom_pieces(_ ,_ ,_ , BOTTOMPIECES, BOTTOMPIECES).

% ldiagonal_top_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -TOPPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position
% The line starts at the position of the first upper left diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece
ldiagonal_top_pieces(GAMESTATE, YS-XS, PIECE, ACC, TOPPIECES) :-
    X1 is XS -1,
    Y1 is YS -1,
    X1 >= 1,
    Y1 >= 1,
    get_piece(GAMESTATE, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    ldiagonal_top_pieces(GAMESTATE, Y1-X1, PIECE, ACC1, TOPPIECES).
ldiagonal_top_pieces(_ ,_ ,_ , TOPPIECES, TOPPIECES).

% ldiagonal_bottom_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -BOTTOMPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position
% The line starts at the position of the first bottom right diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece
ldiagonal_bottom_pieces(GAMESTATE, YS-XS, PIECE, ACC, BOTTOMPIECES) :-
    X1 is XS + 1,
    Y1 is YS + 1,
    boardsize(SIZE),
    X1 =< SIZE,
    Y1 =< SIZE,
    get_piece(GAMESTATE, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    ldiagonal_bottom_pieces(GAMESTATE, Y1-X1, PIECE, ACC1, BOTTOMPIECES).
ldiagonal_bottom_pieces(_ ,_ ,_ , BOTTOMPIECES, BOTTOMPIECES).

% rdiagonal_top_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -TOPPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position
% The line starts at the position of the first upper right diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece
rdiagonal_top_pieces(GAMESTATE, YS-XS, PIECE, ACC, TOPPIECES) :-
    X1 is XS + 1,
    Y1 is YS -1,
    boardsize(SIZE),
    X1 =< SIZE,
    Y1 >= 1,
    get_piece(GAMESTATE, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    rdiagonal_top_pieces(GAMESTATE, Y1-X1, PIECE, ACC1, TOPPIECES).
rdiagonal_top_pieces(_ ,_ ,_ , TOPPIECES, TOPPIECES).

% rdiagonal_bottom_pieces(+GAMESTATE, +(YS-XS), +PIECE, +ACC, -BOTTOMPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position
% The line starts at the position of the first bottom right diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece
rdiagonal_bottom_pieces(GAMESTATE, YS-XS, PIECE, ACC, BOTTOMPIECES) :-
    X1 is XS - 1,
    Y1 is YS + 1,
    boardsize(SIZE),
    X1 >= 1,
    Y1 =< SIZE,
    get_piece(GAMESTATE, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    rdiagonal_bottom_pieces(GAMESTATE, Y1-X1, PIECE, ACC1, BOTTOMPIECES).
rdiagonal_bottom_pieces(_ ,_ ,_ , BOTTOMPIECES, BOTTOMPIECES).

% horizontal_length(+GAMESTATE, +(YS-XS), +PIECE, -LENGTH)
% Calculates the max length of the horizontal move starting at the input position
horizontal_length(GAMESTATE, YS-XS, PIECE, LENGTH) :-
    horizontal_left_pieces(GAMESTATE, YS-XS, PIECE, 0, LEFTPIECES),
    horizontal_right_pieces(GAMESTATE, YS-XS, PIECE, 0, RIGHTPIECES),
    LENGTH is LEFTPIECES + RIGHTPIECES + 1.

% vertical_length(+GAMESTATE, +(YS-XS), +PIECE, -LENGTH)
% Calculates the max length of the vertical move starting at the input position
vertical_length(GAMESTATE, YS-XS, PIECE, LENGTH) :-
    vertical_top_pieces(GAMESTATE, YS-XS, PIECE, 0, TOPPIECES),
    vertical_bottom_pieces(GAMESTATE, YS-XS, PIECE, 0, BOTTOMPIECES),
    LENGTH is TOPPIECES + BOTTOMPIECES + 1.

% ldiagonal_length(+GAMESTATE, +(YS-XS), +PIECE, -LENGTH)
% Calculates the max length of the left diagonal move starting at the input position
ldiagonal_length(GAMESTATE, YS-XS, PIECE, LENGTH) :-
    ldiagonal_top_pieces(GAMESTATE, YS-XS, PIECE, 0, TOPPIECES),
    ldiagonal_bottom_pieces(GAMESTATE, YS-XS, PIECE, 0, BOTTOMPIECES),
    LENGTH is TOPPIECES + BOTTOMPIECES + 1.

% rdiagonal_length(+GAMESTATE, +(YS-XS), +PIECE, -LENGTH)
% Calculates the max length of the right diagonal move starting at the input position
rdiagonal_length(GAMESTATE, YS-XS, PIECE, LENGTH) :-
    rdiagonal_top_pieces(GAMESTATE, YS-XS, PIECE, 0, TOPPIECES),
    rdiagonal_bottom_pieces(GAMESTATE, YS-XS, PIECE, 0, BOTTOMPIECES),
    LENGTH is TOPPIECES + BOTTOMPIECES + 1.

% get_move_maxlength(+GAMESTATE, +START, +PIECE, +DIRECTION, -MAXLENGTH)
% Calculates the move max length using its direction
get_move_maxlength(GAMESTATE, START, PIECE, DIRECTION, MAXLENGTH) :-
    (DIRECTION = horizontal -> 
        horizontal_length(GAMESTATE, START, PIECE, MAXLENGTH)
    ; DIRECTION = vertical -> 
        vertical_length(GAMESTATE, START, PIECE, MAXLENGTH)
    ; DIRECTION = ldiagonal -> 
        ldiagonal_length(GAMESTATE, START, PIECE, MAXLENGTH)
    ; DIRECTION = rdiagonal -> 
       rdiagonal_length(GAMESTATE, START, PIECE, MAXLENGTH)
    ; DIRECTION = invalid ->
        MAXLENGTH = -1
    ).

% get_move_length(+[START, END], -LENGTH)
% Gets the move length
get_move_length([YS-XS, YF-XF], LENGTH) :-
    DIFFX is abs(XF - XS),
    DIFFY is abs(YF - YS),
    LENGTH is max(DIFFX, DIFFY).

% valid_piece(+PLAYER, +PIECE)
% Validates if the piece that player is trying to move is valid or not
valid_piece(PLAYER, PIECE) :-
    symbol(PLAYERPIECE,PLAYER),
    PLAYERPIECE = PIECE.

% valid_fpiece(+PLAYER, +FPIECE)
% Validates if the piece in the landing position is valid or not
% If is an enemy piece or empty is valid, otherwise invalid
valid_fpiece(PLAYER, FPIECE) :-
    symbol(PLAYERPIECE,PLAYER),
    (PLAYERPIECE \= FPIECE ; FPIECE = 0).

% valid_direction(+DIRECTION)
% Validates if the move has a valid direction
valid_direction(DIRECTION) :-
    DIRECTION \= invalid.

% valid_length(+LENGTH, +MAXLENGTH)
% Validates if the move length is lower or equal to the move max length
valid_length(LENGTH, MAXLENGTH) :-
    LENGTH >= 1, MAXLENGTH >= 1, LENGTH =< MAXLENGTH.

% valid_move(+PLAYER, +PIECE, +FPIECE, +LENGTH, +MAXLENGTH, +DIRECTION)
% Validates if a move is valid by using many auxiliar valid move predicates
valid_move(PLAYER, PIECE, FPIECE, LENGTH, MAXLENGTH, DIRECTION) :-
    valid_piece(PLAYER, PIECE),
    valid_fpiece(PLAYER, FPIECE),
    valid_direction(DIRECTION),
    valid_length(LENGTH, MAXLENGTH).

