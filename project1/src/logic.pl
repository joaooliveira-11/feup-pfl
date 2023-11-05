:- consult(data).

% execute_move(+GAMESTATE, +MOVE, -NEWGAMESTATE)
% Executes a move by replacing the piece in the position 'END' by the piece at the position 'START' and clears the 'START' position.
% Returns a new game state with the updated board.
execute_move(GAMESTATE, [START, END], NEWGAMESTATE) :-
    [BOARD, SIZE, PLAYER, GAMEMODE, BOTLEVEL] = GAMESTATE,
    get_piece(BOARD, START, PIECE),
    add_piece(BOARD, START, 0, TEMPBOARD),
    add_piece(TEMPBOARD, END, PIECE, NEWGAMEBOARD),
    NEWGAMESTATE = [NEWGAMEBOARD,SIZE, PLAYER, GAMEMODE, BOTLEVEL].

% get_piece(+BOARD, +(Y-X), -PIECE)
% Gets the piece in the specified position.
get_piece(BOARD, Y-X, PIECE) :-
    nth1(Y, BOARD, ROW),
    nth1(X, ROW, PIECE).

% add_piece(+BOARD, +(Y-X), +PIECE, -NEWGAMEBOARD)
% Gets the board row that matches the y coordinates.
% Replaces the collumn x of that row with the input piece.
% Replaces the row with the modified piece into the board.
% Returns a new board with the updated piece.
add_piece(BOARD, Y-X, PIECE, NEWGAMEBOARD) :-
    get_row(BOARD, Y, ROW),
    replace_piece(X, ROW, PIECE, NEWROW),
    replace_row(Y, BOARD, NEWROW, NEWGAMEBOARD).

% get_row(+BOARD, +INDEX, -SELECTEDROW)
% Gets a board row in the specified index.
get_row(BOARD, INDEX, SELECTEDROW) :-
    nth1(INDEX, BOARD, SELECTEDROW).

% replace_piece(+INDEX, +ROW, +PIECE, -NEWROW)
% Replaces the piece at the index input of the row input by the piece input.
% Returns a new row with the updated piece.
replace_piece(INDEX, ROW, PIECE, NEWROW) :-
    nth1(INDEX, ROW, _, TEMPROW),
    nth1(INDEX, NEWROW, PIECE, TEMPROW).

% replace_row(+INDEX, +BOARD, +PIECE, -NEWBOARD)
% Replaces the row in the index input of the board by the new row with the piece.
% Returns a new board with the updated row.
replace_row(INDEX, BOARD, PIECE, NEWBOARD) :-
    nth1(INDEX, BOARD, _, TEMPBOARD),  % Discard the old element at Index
    nth1(INDEX, NEWBOARD, PIECE, TEMPBOARD).

% get_direction(+(YS-XS), +(YF-XF), -direction)
% Gets the move direction.
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

% horizontal_left_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -LEFTPIECES)
% Calculates the ammount of pieces that form a piece line starting at the left position of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece.
horizontal_left_pieces(BOARD, YS-XS, PIECE, ACC, LEFTPIECES) :-
    X1 is XS -1,
    X1 >= 1,
    get_piece(BOARD, YS-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    horizontal_left_pieces(BOARD, YS-X1, PIECE, ACC1, LEFTPIECES), !.
horizontal_left_pieces(_ ,_ ,_ , LEFTPIECES, LEFTPIECES).

% horizontal_right_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -RIGHTPIECES)
% Calculates the ammount of pieces that form a piece line starting at the right position of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece.
horizontal_right_pieces(BOARD, YS-XS, PIECE, ACC, RIGHTPIECES) :-
    X1 is XS + 1,
    boardsize(SIZE),
    X1 =< SIZE,
    get_piece(BOARD, YS-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    horizontal_right_pieces(BOARD, YS-X1, PIECE, ACC1, RIGHTPIECES), !.
horizontal_right_pieces(_ ,_ ,_ , RIGHTPIECES, RIGHTPIECES).

% vertical_top_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -TOPPIECES)
% Calculates the ammount of pieces that form a piece line starting at the above position of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece.
vertical_top_pieces(BOARD, YS-XS, PIECE, ACC, TOPPIECES) :-
    Y1 is YS - 1,
    Y1 >= 1,
    get_piece(BOARD, Y1-XS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    vertical_top_pieces(BOARD, Y1-XS, PIECE, ACC1, TOPPIECES), !.
vertical_top_pieces(_, _, _, TOPPIECES, TOPPIECES).

% vertical_bottom_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -BOTTOMPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position bellow of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece.
vertical_bottom_pieces(BOARD, YS-XS, PIECE, ACC, BOTTOMPIECES) :-
    Y1 is YS + 1,
    boardsize(SIZE),
    Y1 =< SIZE,
    get_piece(BOARD, Y1-XS, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    vertical_bottom_pieces(BOARD, Y1-XS, PIECE, ACC1, BOTTOMPIECES), !.
vertical_bottom_pieces(_ ,_ ,_ , BOTTOMPIECES, BOTTOMPIECES).

% ldiagonal_top_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -TOPPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position.
% The line starts at the position of the first upper left diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece
ldiagonal_top_pieces(BOARD, YS-XS, PIECE, ACC, TOPPIECES) :-
    X1 is XS -1,
    Y1 is YS -1,
    X1 >= 1,
    Y1 >= 1,
    get_piece(BOARD, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    ldiagonal_top_pieces(BOARD, Y1-X1, PIECE, ACC1, TOPPIECES), !.
ldiagonal_top_pieces(_ ,_ ,_ , TOPPIECES, TOPPIECES).

% ldiagonal_bottom_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -BOTTOMPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position.
% The line starts at the position of the first bottom right diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece.
ldiagonal_bottom_pieces(BOARD, YS-XS, PIECE, ACC, BOTTOMPIECES) :-
    X1 is XS + 1,
    Y1 is YS + 1,
    boardsize(SIZE),
    X1 =< SIZE,
    Y1 =< SIZE,
    get_piece(BOARD, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    ldiagonal_bottom_pieces(BOARD, Y1-X1, PIECE, ACC1, BOTTOMPIECES), !.
ldiagonal_bottom_pieces(_ ,_ ,_ , BOTTOMPIECES, BOTTOMPIECES).

% rdiagonal_top_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -TOPPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position.
% The line starts at the position of the first upper right diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece.
rdiagonal_top_pieces(BOARD, YS-XS, PIECE, ACC, TOPPIECES) :-
    X1 is XS + 1,
    Y1 is YS -1,
    boardsize(SIZE),
    X1 =< SIZE,
    Y1 >= 1,
    get_piece(BOARD, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    rdiagonal_top_pieces(BOARD, Y1-X1, PIECE, ACC1, TOPPIECES), !.
rdiagonal_top_pieces(_ ,_ ,_ , TOPPIECES, TOPPIECES).

% rdiagonal_bottom_pieces(+BOARD, +(YS-XS), +PIECE, +ACC, -BOTTOMPIECES)
% Calculates the ammount of pieces that form a piece line starting at the position.
% The line starts at the position of the first bottom right diagonal square of the original position.
% Piece lines are lines formed with only one type of piece, in this case the player piece.
rdiagonal_bottom_pieces(BOARD, YS-XS, PIECE, ACC, BOTTOMPIECES) :-
    X1 is XS - 1,
    Y1 is YS + 1,
    boardsize(SIZE),
    X1 >= 1,
    Y1 =< SIZE,
    get_piece(BOARD, Y1-X1, CURRENTPIECE),
    CURRENTPIECE = PIECE,
    ACC1 is ACC + 1,
    rdiagonal_bottom_pieces(BOARD, Y1-X1, PIECE, ACC1, BOTTOMPIECES), !.
rdiagonal_bottom_pieces(_ ,_ ,_ , BOTTOMPIECES, BOTTOMPIECES).

% horizontal_length(+BOARD, +(YS-XS), +PIECE, -LENGTH)
% Calculates the line length of the horizontal move starting at the input position.
horizontal_length(BOARD, YS-XS, PIECE, LENGTH) :-
    horizontal_left_pieces(BOARD, YS-XS, PIECE, 0, LEFTPIECES),
    horizontal_right_pieces(BOARD, YS-XS, PIECE, 0, RIGHTPIECES),
    LENGTH is LEFTPIECES + RIGHTPIECES + 1.

% vertical_length(+BOARD, +(YS-XS), +PIECE, -LENGTH)
% Calculates the line length of the vertical move starting at the input position.
vertical_length(BOARD, YS-XS, PIECE, LENGTH) :-
    vertical_top_pieces(BOARD, YS-XS, PIECE, 0, TOPPIECES),
    vertical_bottom_pieces(BOARD, YS-XS, PIECE, 0, BOTTOMPIECES),
    LENGTH is TOPPIECES + BOTTOMPIECES + 1.

% ldiagonal_length(+BOARD, +(YS-XS), +PIECE, -LENGTH)
% Calculates the line length of the left diagonal move starting at the input position.
ldiagonal_length(BOARD, YS-XS, PIECE, LENGTH) :-
    ldiagonal_top_pieces(BOARD, YS-XS, PIECE, 0, TOPPIECES),
    ldiagonal_bottom_pieces(BOARD, YS-XS, PIECE, 0, BOTTOMPIECES),
    LENGTH is TOPPIECES + BOTTOMPIECES + 1.

% rdiagonal_length(+BOARD, +(YS-XS), +PIECE, -LENGTH)
% Calculates the line length of the right diagonal move starting at the input position.
rdiagonal_length(BOARD, YS-XS, PIECE, LENGTH) :-
    rdiagonal_top_pieces(BOARD, YS-XS, PIECE, 0, TOPPIECES),
    rdiagonal_bottom_pieces(BOARD, YS-XS, PIECE, 0, BOTTOMPIECES),
    LENGTH is TOPPIECES + BOTTOMPIECES + 1.

% get_move_maxlength(+BOARD, +START, +PIECE, +DIRECTION, -LINELENGTH)
% Calculates the move line length using its direction.
get_move_linelength(BOARD, START, PIECE, DIRECTION, LINELENGTH) :-
    (DIRECTION = horizontal -> 
        horizontal_length(BOARD, START, PIECE, LINELENGTH)
    ; DIRECTION = vertical -> 
        vertical_length(BOARD, START, PIECE, LINELENGTH)
    ; DIRECTION = ldiagonal -> 
        ldiagonal_length(BOARD, START, PIECE, LINELENGTH)
    ; DIRECTION = rdiagonal -> 
       rdiagonal_length(BOARD, START, PIECE, LINELENGTH)
    ; DIRECTION = invalid ->
        LINELENGTH = -1
    ).

% get_move_length(+[START, END], -LENGTH)
% Gets the move length.
get_move_length([YS-XS, YF-XF], LENGTH) :-
    DIFFX is abs(XF - XS),
    DIFFY is abs(YF - YS),
    LENGTH is max(DIFFX, DIFFY).

% valid_piece(+PLAYER, +PIECE, +DISPLAYERRORS)
% Validates if the starting piece that player is trying to move is valid or not.
valid_piece(PLAYER, PIECE, _) :-
    symbol(PLAYERPIECE,PLAYER),
    PLAYERPIECE = PIECE, !.
valid_piece(_,_,1) :-
    write('Invalid starting piece. You can only control your own pieces!\n'), 
    fail.
valid_piece(_,_,0) :- 
    fail.

% valid_fpiece(+PLAYER, +FPIECE, +DISPLAYERRORS)
% Validates if the piece in the landing position is valid or not.
% If is an enemy piece or empty is valid, otherwise invalid.
valid_fpiece(PLAYER, FPIECE,_) :-
    symbol(PLAYERPIECE,PLAYER),
    (PLAYERPIECE \= FPIECE ; FPIECE = 0), !.
valid_fpiece(_,_,1) :-
    write('Invalid landing piece. You cannot land on your own pieces!\n'), 
    fail.
valid_fpiece(_,_,0) :-
    fail.

% valid_direction(+DIRECTION, +DISPLAYERRORS)
% Validates if the move has a valid direction.
valid_direction(DIRECTION, _) :-
    DIRECTION \= invalid, !.
valid_direction(_,1) :-
    write('Invalid direction! You can only use diagonal, vertical, or horizontal directions.\n'), 
    fail.
valid_direction(_,0) :-
    fail.

% valid_length(+LENGTH, +LINELENGTH, +DISPLAYERRORS)
% Validates if the move length is equal to the move line length.
valid_length(LENGTH, LINELENGTH, _) :-
    LENGTH >= 1, LINELENGTH >= 1, LENGTH = LINELENGTH, !.
valid_length(LENGTH,LINELENGTH, 1) :-
    format('Invalid move length. Your move has ~w length, and the line length is ~w. They must be equal!\n', [LENGTH, LINELENGTH]), 
    fail.
valid_length(_,_, 0) :-
    fail.

% valid_coordinates(+(Y-X))
% Validates if the position is inside the board.
valid_coordinates(Y-X):-
    boardsize(SIZE),
    X > 0, X =< SIZE,
    Y > 0, Y =< SIZE.

% move_type(+LENGTH, -TYPE)
% Gets the move type.
move_type(LENGTH, TYPE) :-
    (LENGTH = 1 ->
        TYPE = single_step
    ;
    LENGTH > 1 ->
        TYPE = jump
    ).

% update_continousmove_piece(+PLAYER, +END)
% Updates the continous move piece of the player.
update_continousmove_piece(PLAYER, END) :-
    (jump_piece(PLAYER,_) ->
        retract(jump_piece(PLAYER, _))
    ;
        true
    ),
    assert(jump_piece(PLAYER, END)).

% remove_continousmove_piece(+PLAYER)
% Clears the continous move piece of the player.
remove_continousmove_piece(PLAYER) :-
    retract(jump_piece(PLAYER,_)).

% handle_move_type(+TYPE, +PLAYER, +[START, END])
% Updates the permission for single and continous steps for a player.
% In case of jumps, updates the continous move piece and the blocked positions.
handle_move_type(TYPE, PLAYER, [START, END]) :-
    (TYPE = single_step ->
        allow_single_steps(PLAYER)
    ;
    TYPE = jump ->
        allow_continous_moves(PLAYER),
        update_continousmove_piece(PLAYER, END),
        add_blocked_position(PLAYER, START)
    ).

% allow_single_steps(+PLAYER)
% Updates the permission for single steps.
allow_single_steps(PLAYER) :-
    retract(can_continuous_move(PLAYER,_)),
    assert(can_continuous_move(PLAYER, no)).

% allow_continous_moves(+PLAYER)
% Updates the permission for continous moves.
allow_continous_moves(PLAYER):-
    retract(can_continuous_move(PLAYER, _)),
    assert(can_continuous_move(PLAYER, yes)).

% valid_move_type(+PLAYER, +TYPE, +STARTPOSITION, +DISPLAYERRORS)
% Validates the move type.
valid_move_type(PLAYER, jump, Y-X, _) :-
    (
        (can_continuous_move(PLAYER, yes),
        jump_piece(PLAYER, POSITION),
        Y1-X1 = POSITION,
        Y = Y1, X = X1)
    ;
        can_continuous_move(PLAYER, no)
    ), !.
valid_move_type(PLAYER, jump, _, 1) :-
    jump_piece(PLAYER, POSITION),
    Y-X = POSITION,
    format('Invalid move. Since you are making continuous moves, you can only move the previous piece, now at position (~w,~w)\n', [Y, X]), 
    fail.
valid_move_type(_, jump, _, 0) :-
    fail.
valid_move_type(PLAYER, single_step, _, _) :-
    can_continuous_move(PLAYER, no), !.
valid_move_type(_, single_step, _, 1) :-
    write('Invalid move. Since you are making continous moves, single steps are not allowed!\n'),
    fail.
valid_move_type(_, single_step, _, 0) :-
    fail.

% check_white_first_move(+PLAYER)
% Checks if the move is the first one from the white side.
check_white_first_move(PLAYER) :-
    PLAYER = 'W',
    first_move(PLAYER),
    write('Since is your first move as a white side, you cannot play again even with a jump move.\n'),
    update_white_first_move(PLAYER).

% update_white_first_move(+PLAYER)
% Updates the white first turn, removing it from the fact base.
update_white_first_move(PLAYER) :-
    retract(first_move(PLAYER)).

% add_blocked_position(+PLAYER, +POSITION)
% Updates the player list of blocked positions, adding the position received as argument.
add_blocked_position(PLAYER, POSITION) :-
    (PLAYER = 'W' -> 
        retract(white_blocked_positions(POSITIONS)),
        assert(white_blocked_positions([POSITION | POSITIONS]))
    ;
    PLAYER = 'B' -> 
        retract(black_blocked_positions(POSITIONS)),
        assert(black_blocked_positions([POSITION | POSITIONS]))
    ).

% clear_blocked_positions(+PLAYER)
% Clears the player blocked positions.
clear_blocked_positions(PLAYER) :-
     (PLAYER = 'W' -> 
        retract(white_blocked_positions(_)),
        assert(white_blocked_positions([]))
    ;
    PLAYER = 'B' -> 
        retract(black_blocked_positions(_)),
        assert(black_blocked_positions([]))
     ).

% valid_position(+PLAYER, +END, +DISPLAYERRORS)
% Validates if the end position of the move is a blocked position.
valid_position(PLAYER, END, _) :-
     (PLAYER = 'W' -> 
        white_blocked_positions(BLOCKEDPOSITIONS)
    ;
    PLAYER = 'B' -> 
        black_blocked_positions(BLOCKEDPOSITIONS)
     ),
     \+ member(END, BLOCKEDPOSITIONS), !.
valid_position(_, END, 1) :-
    format("Error, the square ~w is a blocked position in this turn.~n", [END]), 
    fail.
valid_position(_, _, 0) :-
    fail.

% valid_move(+BOARD, +PLAYER, +[START, END], -TYPE, +DISPLAYERRORS)
% Validates if a move is valid.
valid_move(BOARD, PLAYER, [START, END], TYPE, DISPLAYERRORS) :-
    get_piece(BOARD, START, PIECE),
    valid_piece(PLAYER, PIECE, DISPLAYERRORS),
    get_piece(BOARD, END, FPIECE),
    valid_fpiece(PLAYER, FPIECE, DISPLAYERRORS),
    get_direction(START, END, DIRECTION),
    valid_direction(DIRECTION, DISPLAYERRORS),
    get_move_linelength(BOARD, START, PIECE, DIRECTION, LINELENGTH),
    get_move_length([START, END], LENGTH),
    valid_length(LENGTH, LINELENGTH, DISPLAYERRORS),
    move_type(LENGTH, TYPE),
    valid_move_type(PLAYER, TYPE, START, DISPLAYERRORS),
    valid_position(PLAYER, END, DISPLAYERRORS).

% get_player_positions(+BOARD, +PLAYER, +SIZE, -POSITIONS)
% Gets the positions of player pieces.
get_player_positions(BOARD, PLAYER, SIZE, POSITIONS) :-
    findall([Y, X], (between(1, SIZE, Y), nth1(Y, BOARD, ROW), between(1, SIZE, X), nth1(X, ROW, PIECE),  symbol(PLAYERPIECE, PLAYER), PLAYERPIECE =:= PIECE), POSITIONS).

% check_positions(+BOARD, +PLAYER, +POSITIONS)
% Checks if all pieces in the positions list are vertically, horizontally, and diagonally free from the pieces of the player received as argument.
check_positions(_, _, []).
check_positions(BOARD, PLAYER, [[Y, X] | REST]) :-
    check_position(BOARD, PLAYER, [Y, X]),
    check_positions(BOARD,PLAYER, REST).

% check_position(+BOARD, +PLAYER, +[Y,X])
% Checks if a position received as argument is vertically, horizontally, and diagonally free from the pieces of the player received as argument.
% Greedy way of only checking 4 positions per piece instead of 8 by taking advantage of how they are presented in the list.
check_position(BOARD, PLAYER,[Y,X]) :-
    X1 is X + 1,
    X2 is X - 1,
    Y1 is Y + 1,
    \+ (has_friendly_piece(BOARD, PLAYER, [Y,X1]);
        has_friendly_piece(BOARD, PLAYER, [Y1,X]); 
        has_friendly_piece(BOARD, PLAYER, [Y1,X1]);
        has_friendly_piece(BOARD, PLAYER, [Y1,X2])
        ).

% check_position(+BOARD, +PLAYER, +[Y,X])
% Auxiliar predicate used for the game state evaluation.
% In game state evaluation we dont use the greedy way since we count the number of isolated positions and not if all are isolated.
% Therefore, we cant take advantage of the previous pieces in the list.
check_position_2(BOARD, PLAYER,[Y,X]) :-
    X1 is X + 1,
    X2 is X - 1,
    Y1 is Y - 1,
    \+ (has_friendly_piece(BOARD, PLAYER, [Y,X2]);
        has_friendly_piece(BOARD, PLAYER, [Y1,X]); 
        has_friendly_piece(BOARD, PLAYER, [Y1,X1]);
        has_friendly_piece(BOARD, PLAYER, [Y1,X2])
        ).

% has_friendly_piece(+BOARD, +PLAYER, +[Y,X])
% Auxiliar predicate for the check_position/3.
has_friendly_piece(BOARD, PLAYER,[Y,X]) :-
    (valid_coordinates(Y-X) -> 
        get_piece(BOARD, Y-X, PIECE),
        symbol(PLAYERPIECE, PLAYER),
        PLAYERPIECE =:= PIECE
    ;
        false
    ).

% check_win(+GAMESTATE)
% Checks if the current game state result in a win for a player.
check_win(GAMESTATE) :-
    [BOARD, SIZE, PLAYER, _,_] = GAMESTATE,
    get_player_positions(BOARD, PLAYER, SIZE, POSITIONS),
    check_positions(BOARD, PLAYER, POSITIONS).

both_players_win(GAMESTATE) :-
    [BOARD, SIZE, PLAYER, GAMEMODE, BOTLEVEL] = GAMESTATE,
    change_turn(PLAYER, NEXTPLAYER),
    check_win(GAMESTATE),
    NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL], 
    check_win(NEWGAMESTATE).

% continue_game(+GAMESTATE)
% Analises if the player can play again.
% If the player can play again doesnt change the turn and gets the answer.
% Otherwise, changes the turn and call the play_game/1 cycle with the updated game state
continue_game(GAMESTATE) :-
    [BOARD, SIZE, PLAYER, GAMEMODE, BOTLEVEL] = GAMESTATE,
    (check_white_first_move(PLAYER) ->
        allow_single_steps(PLAYER),
        clear_blocked_positions(PLAYER),
        remove_continousmove_piece(PLAYER),
        change_turn(PLAYER, NEXTPLAYER),
        NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL],
        play_game(NEWGAMESTATE)
    ;
    can_continuous_move(PLAYER, yes)->
        valid_moves(GAMESTATE, PLAYER, VALIDMOVES),
        (VALIDMOVES \= [] -> ask_to_play_again(GAMESTATE, GAMEMODE)
        ;
        allow_single_steps(PLAYER),
        clear_blocked_positions(PLAYER),
        remove_continousmove_piece(PLAYER),
        change_turn(PLAYER, NEXTPLAYER),
        NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL],
        play_game(NEWGAMESTATE)
        )
    ;
    can_continuous_move(PLAYER, no) ->
        change_turn(PLAYER, NEXTPLAYER),
        NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL],
        play_game(NEWGAMESTATE)
    ).

% game_over(+GAMESTATE, -WINNER)
% Checks if any player won the game.
% First checks the other player because if both win at the same time, the current player that made the move loses.
game_over(GAMESTATE, WINNER) :-
    [BOARD, SIZE, PLAYER, GAMEMODE, BOTLEVEL] = GAMESTATE,
    change_turn(PLAYER, NEXTPLAYER),
    NEWGAMESTATE = [BOARD, SIZE, NEXTPLAYER, GAMEMODE, BOTLEVEL],
    (both_players_win(GAMESTATE) ->
        WINNER = NEXTPLAYER
    ;
    check_win(GAMESTATE) ->
        WINNER = PLAYER
    ;
    check_win(NEWGAMESTATE) ->
        WINNER = NEXTPLAYER
    ).

% valid_moves(+GAMESTATE, +PLAYER, -VALIDMOVES)
% Gets all valid moves from a player at a specific game state.
valid_moves(GAMESTATE, PLAYER, VALIDMOVES) :-
    [BOARD,SIZE, _, _, _] = GAMESTATE,
    get_player_positions(BOARD, PLAYER, SIZE, POSITIONS),
    findall(
        [YS-XS, YF-XF],
        (
        member([YS,XS], POSITIONS),
        between(1, SIZE, YF),
        between(1, SIZE, XF),
        valid_move(BOARD, PLAYER, [YS-XS, YF-XF],_,0)
        ),
        VALIDMOVES
    ).

% value(+GAMESTATE, +PLAYER, -VALUE)
% Gets the value from a specific game state
% Consider the value to be the number of isolated pieces from the player + (2* number of other player pieces)
% We consider a trade_off of 2 enemy pieces, prioritizing their existence in moves that remove them from the game.
value(GAMESTATE, PLAYER, VALUE) :-
    [BOARD, SIZE, _, _, _] = GAMESTATE,
    get_player_positions(BOARD, PLAYER, SIZE, POSITIONS),
    findall(
        [Y, X],
        (   
            member([Y,X], POSITIONS),
            check_position(BOARD, PLAYER,[Y,X]),
            check_position_2(BOARD, PLAYER,[Y,X])
        ),
        ISOLATEDPOSITIONS
    ),
    length(ISOLATEDPOSITIONS, ISOLATEDPOSITIONSVALUE),
    change_turn(PLAYER, NEXTPLAYER),
    get_player_positions(BOARD, NEXTPLAYER, SIZE, OTHERPOSITIONS),
    length(OTHERPOSITIONS, ENEMYPIECES),
    TRADE_OFF_FACTOR is 2,
    VALUE is (ISOLATEDPOSITIONSVALUE + (TRADE_OFF_FACTOR * ENEMYPIECES)).