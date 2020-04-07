:- consult(environment).
:- consult(punctuation).

:- (dynamic ending_move/1).

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


random_strategy(_, All_moves, Source, Color, Amount, Stair, Chip) :-
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


%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%not(is_game_move);is_winning(ID)
is_winning(Player) :-
    setof((Score, ID), player_score(ID, Score), Scores),
    last(Scores,  (_, Player)).

is_game_move(ID, I, Color) :-
    calculate_points_horizontal(ID, I, Color, Amount),
    Amount=:=5.

is_somebody_ending_game() :-
    ending_move(_), !.


%Free is the amount of free space in Stair of player ID
get_free_space(ID, Stair, Free) :-
    findall(1,
            ( stair(ID, Stair, _, V),
              V=:=0
            ),
            Available),
    length(Available, Free).

get_adyacents(ID, I, C, Adyacents) :-
    calculate_points_horizontal(ID, I, C, H),
    calculate_points_vertical(ID, I, C, V),
    Adyacents is H+V. 

%Leaves in Moves a list sorted by adyacency in the wall that contains the moves that fill a stair with K extra tiles
get_moves_overfill(ID, All_moves, K, Moves) :-
    setof((Ady, Src, Clr, Amnt, St, Ch),
          Free^(member((Src, Clr, Amnt, St, Ch), All_moves), get_free_space(ID, St, Free), Amnt-Free=:=K, get_adyacents(ID, St, Clr, Ady)),
          Moves).

%Leaves in Moves a list of moves that dont fill a stair sorted by incomplete space in the stair 
get_moves_incomplete(ID, All_moves, Moves) :-
    setof((Incomplete, Src, Clr, Amnt, St, Ch),
          Free^(member((Src, Clr, Amnt, St, Ch), All_moves), get_free_space(ID, St, Free), Incomplete is Free-Amnt, Incomplete>0),
          Moves).

%Leaves in Moves a list of moves that dont fill a stair that already have some tiles on it sorted by incomplete space in the stair 
get_moves_incomplete_to_nonempty(ID, All_moves, Moves) :-
    setof((Incomplete, Src, Clr, Amnt, St, Ch),
          Free^(member((Src, Clr, Amnt, St, Ch), All_moves), get_free_space(ID, St, Free), Incomplete is Free-Amnt, Incomplete>0, stair(ID, St, 1, Clr)),
          Moves).



%Get the move that completely fill a stair and maximize the number of adyacencies in the wall 
strategy(ID, All_moves,Source, Color, Amount, Stair, Chip) :-
    get_moves_overfill(ID,All_moves, 0, Moves),
    last(Moves,
         (_, Source, Color, Amount, Stair, Chip)),
         (not(is_game_move(ID,Stair,Color));is_winning(ID);is_somebody_ending_game()),
          ! .%, print("OPTION1").
%Get the move that overfill a stair by 1 and maximize the number of adyacencies in the wall 
strategy(ID,All_moves, Source, Color, Amount, Stair, Chip) :-
    get_moves_overfill(ID,All_moves, 1, Moves),
    last(Moves,
         (_, Source, Color, Amount, Stair, Chip)),(not(is_game_move(ID,Stair,Color));is_winning(ID);is_somebody_ending_game()), ! .%,print("OPTION2").
%Get the move that overfill a stair by 2 and maximize the number of adyacencies in the wall 
strategy(ID,All_moves, Source, Color, Amount, Stair, Chip) :-
    get_moves_overfill(ID,All_moves, 2, Moves),
    last(Moves,
         (_, Source, Color, Amount, Stair, Chip)),(not(is_game_move(ID,Stair,Color));is_winning(ID);is_somebody_ending_game()), ! .%,print("OPTION3").
%Get the move that underfill a stair that is not empty and minimize the number of empty spaces in the stair
strategy(ID, All_moves, Source, Color, Amount, Stair, Chip) :-
    get_moves_incomplete_to_nonempty(ID,
                                     All_moves,
                                     
                                     [ (_, Source, Color, Amount, Stair, Chip)
                                     | _
                                     ]), !.%,    print("OPTION4").
%Get the move that underfill a stair and minimize the number of empty spaces in the stair
strategy(ID, All_moves, Source, Color, Amount, Stair, Chip) :-
    get_moves_incomplete(ID,
                         All_moves,
                         
                         [ (_, Source, Color, Amount, Stair, Chip)
                         | _
                         ]), !.%,    print("OPTION5").
%Get the move with the least number of extra tiles and does not end the game
strategy(ID, All_moves, Source, Color, Amount, Stair, Chip) :-
    setof((Extra, Src, Clr, Amnt, St, Ch),
          Free^(member((Src, Clr, Amnt, St, Ch), All_moves), get_free_space(ID, St, Free),Extra is Amnt-Free,Extra>=0, not(is_game_move(ID, St, Clr))),
          
          [ (_, Source, Color, Amount, Stair, Chip)
          | _
          ]), ! .%,    print("OPTION6").
%Get the move with the least number of extra tiles
strategy(ID, All_moves, Source, Color, Amount, Stair, Chip) :-
    setof((Extra, Src, Clr, Amnt, St, Ch),
          Free^(member((Src, Clr, Amnt, St, Ch),
                 All_moves),get_free_space(ID, St, Free),Extra is Amnt-Free),
          
          [ (_, Source, Color, Amount, Stair, Chip)
          | _
          ]).%,    print("OPTION7").