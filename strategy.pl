:- consult(environment).

%Color Definition
% 1 - Blue
% 2- Yellow
% 3 - Red 
% 4 - Green
% 5 - White

%Get all factible movements involving one color in factories---------------------------
moves_B(ID, Blues) :-
    findall((Source, 1, Amount, Stair, 0),
            ( factory(Source,
                      Amount,
                      _,
                      _,
                      _,
                      _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 1, Stair)
            ),
            Blues).

moves_Y(ID, Yellows) :-
    findall((Source, 2, Amount, Stair, 0),
            ( factory(Source,
                      _,
                      Amount,
                      _,
                      _,
                      _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 2, Stair)
            ),
            Yellows).

moves_R(ID, Reds) :-
    findall((Source, 3, Amount, Stair, 0),
            ( factory(Source,
                      _,
                      _,
                      Amount,
                      _,
                      _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 3, Stair)
            ),
            Reds).

moves_G(ID, Greens) :-
    findall((Source, 4, Amount, Stair, 0),
            ( factory(Source,
                      _,
                      _,
                      _,
                      Amount,
                      _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 4, Stair)
            ),
            Greens).

moves_W(ID, Whites) :-
    findall((Source, 5, Amount, Stair, 0),
            ( factory(Source,
                      _,
                      _,
                      _,
                      _,
                      Amount),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 5, Stair)
            ),
            Whites).
%-------------------------------------------------------------------------------------------------

%Get all factible movements involving one color in the center of the table------------------------
moves_center_B(ID, Center_Blues) :-
    findall((0, 1, Amount, Stair, Chip),
            ( center(Chip,
                     Amount,
                     _,
                     _,
                     _,
                     _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 1, Stair)
            ),
            Center_Blues).

moves_center_Y(ID, Center_Yellows) :-
    findall((0, 2, Amount, Stair, Chip),
            ( center(Chip,
                     _,
                     Amount,
                     _,
                     _,
                     _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 2, Stair)
            ),
            Center_Yellows).

moves_center_R(ID, Center_Reds) :-
    findall((0, 3, Amount, Stair, Chip),
            ( center(Chip,
                     _,
                     _,
                     Amount,
                     _,
                     _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 3, Stair)
            ),
            Center_Reds).

moves_center_G(ID, Center_Greens) :-
    findall((0, 4, Amount, Stair, Chip),
            ( center(Chip,
                     _,
                     _,
                     _,
                     Amount,
                     _),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 4, Stair)
            ),
            Center_Greens).

moves_center_W(ID, Center_Whites) :-
    findall((0, 5, Amount, Stair, Chip),
            ( center(Chip,
                     _,
                     _,
                     _,
                     _,
                     Amount),
              Amount=\=0,
              member(Stair, [0, 1, 2, 3, 4, 5]),
              factible_move(ID, 5, Stair)
            ),
            Center_Whites).
%------------------------------------------------------------------------------------------------

%Put in All_moves all the factibles moves in an environment state
get_moves(ID, All_moves) :-
    moves_B(ID, Blues),
    moves_Y(ID, Yellows),
    moves_R(ID, Reds),
    moves_G(ID, Greens),
    moves_W(ID, Whites),
    moves_center_B(ID, Center_Blues),
    moves_center_Y(ID, Center_Yellows),
    moves_center_R(ID, Center_Reds),
    moves_center_G(ID, Center_Greens),
    moves_center_W(ID, Center_Whites), 
    append(
           [ Blues,
             Yellows,
             Reds,
             Greens,
             Whites,
             Center_Blues,
             Center_Yellows,
             Center_Reds,
             Center_Greens,
             Center_Whites
           ],
           All_moves).
    % print(All_moves).

%Determine if is possible to place Color at Stair in the board of the player ID
factible_move(_, _, 0).  %Move to floor is always factible
factible_move(ID, Color, Stair) :-
    stair(ID, Stair, Stair, 0),
    (   stair(ID, Stair, 1, Color)
    ;   stair(ID, Stair, 1, 0)
    ),
    cell(ID, Stair, _, Color, 0).

%Selects the R movement in a list of all factible movements and place the result in Source, Color, Amount, Stair, Chip 
select_move(1, [(Source, Color, Amount, Stair, Chip)|_], Source, Color, Amount, Stair, Chip) :- !.
select_move(R, [_|Moves], Source, Color, Amount, Stair, Chip) :-
    K is R-1,
    select_move(K,
                Moves,
                Source,
                Color,
                Amount,
                Stair,
                Chip). 


strategy(ID, Source, Color, Amount, Stair, Chip) :-
    get_moves(ID, All_moves),
    length(All_moves, L),
    N is L+1,
    random(1, N, R),
    select_move(R,
                All_moves,
                Source,
                Color,
                Amount,
                Stair,
                Chip).
