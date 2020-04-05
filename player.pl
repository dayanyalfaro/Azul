:- consult(matrix).
:- consult(environment).
:- consult(strategy).
:- consult(punctuation).

:- dynamic(player_wall/2, player_stair/2, player_penalty/2).

%-----------------Player ATTR----------------------------
set_score(ID, SCORE) :-
    (   retract(player_score(ID, _)), !
    ;   true
    ),
    assert(player_score(ID, SCORE)).
set_penalty(ID, PENALTY) :-
    (   retract(player_penalty(ID, _)), !
    ;   true
    ),
    assert(player_penalty(ID, PENALTY)).

%-----------------Player Initialization--------------------
init_player(ID) :-
    set_score(ID, 0),
    init_board(ID),
    set_penalty(ID, 0). 

create_players(0):-!.
create_players(Cant) :- init_player(Cant), Id is Cant - 1, create_players(Id). 

%-----------------Player Actions---------------------------

%-----(Fase 1)----Player action: pick a movement and execute it 

%Update center after player takes tiles from it
update_environment(0,Color,Chip):-!, remove_tiles_center(Color,_),(Chip =:= 1 -> remove_chip_center();true).
%Move the rest of tiles to the table center after the player take the selected tiles from factory Source
update_environment(Source, Color, _) :-
    remove_tiles_factory(Source, Color, _),
    remove_B_factory(Source, B),
    remove_Y_factory(Source, Y),
    remove_R_factory(Source, R),
    remove_G_factory(Source, G),
    remove_W_factory(Source, W),
    add_tile_center(1, B),
    add_tile_center(2, Y),
    add_tile_center(3, R),
    add_tile_center(4, G),
    add_tile_center(5, W).


%Place tile by tile on the lid
update_lid(Amount, _) :- Amount =< 0.  %SEE: debe haber un corte
update_lid(Amount, Color) :-
    Amount > 0,
    add_tile_lid(Color),
    K is Amount-1,
    update_lid(K, Color).

%Put the fist player chip in the floor if it was taken 
%TODO set player ID as first_player
place_chip(_, 0) :- !.
place_chip(ID, 1) :- set_first_player(ID), write('\n Player: '), write(ID), write('takes the initial tile from the center\n'),
    place_extras(ID, -1, 1).

%Put a total of Amount tiles of color Color,tile by tile,on the floor  
update_floor(_, _, _, 0) :- !. 
update_floor(_, [], _, _) :- !. 
update_floor(ID, [P|Unset], Color, Amount) :-
    set_value_floor(ID, P, Color),
    K is Amount-1,
    update_floor(ID, Unset, Color, K).

%Place the extra tiles of the stair on the floor
place_extras(_, _, Extra) :-
    Extra=<0, !.                        %SEE: CORTE AQUI
place_extras(_, Color, Extra) :- %Poner ID
    Extra>0,
    % findall(P, floor(ID, P, 0, _), Unset),
    % update_floor(ID, Unset, Color, Extra),
    % length(Unset, L),
    % Garbage is Extra-L,
    % update_lid(Garbage, Color).
    update_lid(Extra,Color ).


%Place color tiles one by one on a stair
update_stair(_, _, _, _, 0) :- !.     
update_stair(_, _, [], _, _) :- !.
update_stair(ID, Stair, [P|Unset], Color, Amount) :-
    set_value_stair(ID, Stair, P, Color),
    K is Amount-1,
    update_stair(ID, Stair, Unset, Color, K).

%Fill a stair according to the amount and color of the taken tiles and place the extra tiles on the floor
place_colors(ID, 0, Color, Amount) :-
    !, place_extras(ID, Color, Amount).
place_colors(ID, Stair, Color, Amount) :-
    setof(P, stair(ID, Stair, P, 0), Unset),
    update_stair(ID, Stair, Unset, Color, Amount),
    length(Unset, L),
    Extra is Amount-L,
    place_extras(ID, Color, Extra).

%Pick a movement and execute it 
pick(ID) :-
    get_moves(ID, All_moves),
    random_strategy(ID,
             All_moves,
             Source,
             Color,
             Amount,
             Stair,
             Chip),
    % (   not(is_game_move(ID, Stair, Color)); assert(ending_move(ID))
    % ),
    print([Source, Color, Amount, Stair, Chip]),

    update_environment(Source, Color, Chip),
   
    place_chip(ID, Chip),
   
    place_colors(ID, Stair, Color, Amount).




%-----(Fase 2)----Player action: build the wall

%Fill the stair with 0 and put the extra tiles on the lid
clean_stair(ID, Stair, Color) :-
    setof(P, stair(ID, Stair, P, Color), Unset),
    update_stair(ID, Stair, Unset, 0, Stair),
    Garbage is Stair-1,
    update_lid(Garbage, Color). 



%Update the score of player ID according to the amount of verticals and horizontals adyacents
update_score(ID,1,1):- !,player_score(ID,Current_Score),Score is Current_Score + 1,set_score(ID,Score).
update_score(ID,1,Verticals):-!,player_score(ID,Current_Score),Score is Current_Score + Verticals,set_score(ID,Score). 
update_score(ID, Horizontals, 1) :-!,
    player_score(ID, Current_Score),
    Score is Current_Score+Horizontals,
    set_score(ID, Score).
update_score(ID,Horizontals,Verticals):- player_score(ID, Current_Score),
    Score is Current_Score+Horizontals+Verticals,
    set_score(ID, Score). 


%Goes stair by stair, if stair is filled put one chip on the wall and clean the extra tiles on the stair
build_and_clean(_, []) :- !.
build_and_clean(ID, [(Stair, Color)|Stairs]) :-
    set_value_wall(ID, Stair, _, Color, 1),
    calculate_points_horizontal(ID,Stair,Color,Horizontals),
    calculate_points_vertical(ID,Stair,Color,Verticals),
    update_score(ID,Horizontals,Verticals),
    clean_stair(ID, Stair, Color),
    build_and_clean(ID, Stairs).

%Discount the respective penalty points for every tile in the floor
floor_penalty(ID) :-
    findall(P,
            ( floor(ID, _, V, P),
              V=\=0
            ),
            Penalties),
    sum_list(Penalties, Sum),
    player_score(ID, Current_Score),
    Score is Current_Score+Sum,
    set_score(ID, Score),
    (   Score<0
    ->  set_score(ID, 0)
    ;   true
    ).


%Get the most left position of a stair that is colored ( means it is complete) and updates the wall and stairs
build_wall(ID) :-
    setof((Stair, Color),
          (stair(ID, Stair, Stair, Color), Color=\=0),Stairs),
    build_and_clean(ID, Stairs),
    floor_penalty(ID).


update_all_walls(0):- !.
update_all_walls(Cant):- build_wall(Cant), ID is Cant - 1, update_all_walls(ID).



%Check if a line Row in the wall has L tiles completed.
check_line(ID, Row,  L):- findall(_, cell(ID, Row, _, _, 1), T), length(T, L ). 

check_all_lines(ID, Row) :-  Row =< 5, check_line(ID, Row, 5).
check_all_lines(ID, Row) :-  Row =< 5, R2 is Row + 1, check_all_lines(ID, R2).

%Check if the number of completed lines in the wall is > 0
check_stop_player(ID) :- check_all_lines(ID, 1).

check_stop(ID) :- cant_players(Cant), ID =< Cant, check_stop_player(ID), !.
check_stop(ID) :- cant_players(Cant), ID =< Cant, ID1 is ID+1 , check_stop(ID1).

