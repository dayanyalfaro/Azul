:- consult(matrix).
:- consult(environment).
:- consult(strategy).

:- dynamic(player_score/2, player_wall/2, player_stair/2, player_penalty/2).

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

%-----------------Player Actions---------------------------

%-----(Fase 1)----Player action: pick a movement and execute it 

%Update factory or table center after player takes tiles
update_environment(0,Color,Chip):- remove_tiles_center(Color,_),Chip =:= 1 -> remove_chip_center().
update_environment(Source, Color, _) :-
    remove_tiles_factory(Source, Color, _).

%Place tile by tile on the lid
update_lid(0, _) :- !.
update_lid(Amount, Color) :-
    add_tile_lid(Color),
    K is Amount-1,
    update_lid(K, Color).

%Put the fist player chip in the floor if it was taken 
%TODO set player ID as first_player
place_chip(_, 0) :- !.
place_chip(ID, 1) :-
    place_extras(ID, -1, 1).

%Put a total of Amount tiles of color Color,tile by tile,on the floor   
update_floor(_, _, _, 0) :- !. 
update_floor(_, [], _, _) :- !. 
update_floor(ID, [P|Unset], Color, Amount) :-
    floor(ID, P, _, Penalty),
    set_value_floor(ID, P, Color, Penalty),
    K is Amount-1,
    update_floor(ID, Unset, Color, K).

%Place the extra tiles of the stair on the floor
place_extras(_, _, Extra) :-
    Extra=<0, !.
place_extras(ID, Color, Extra) :-
    Extra>0,
    setof(P, floor(ID, P, 0, _), Unset),
    update_floor(ID, Unset, Color, Extra),
    length(Unset, L),
    Garbage is Extra-L,
    update_lid(Garbage, Color).

%Place color tiles one by one on a stair
update_stair(_, _, _, _, 0) :- !.
update_stair(_, _, [], _, _) :- !.
update_stair(ID, Stair, [P|Unset], Color, Amount) :-
    set_value_stair(ID, Stair, P, Color),
    K is Amount-1,
    update_stair(ID, Stair, Unset, Color, K).

%Fill a stair according to the amount and color of the taken tiles and place the extra tiles on the floor
place_colors(ID, Stair, Color, Amount) :-
    setof(P, stair(ID, Stair, P, 0), Unset),
    update_stair(ID, Stair, Unset, Color, Amount),
    length(Unset, L),
    Extra is Amount-L,
    place_extras(ID, Color, Extra).

%Pick a movement and execute it 
pick(ID) :-
    strategy(Source, Color, Amount, Stair, Chip),
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

%Goes stair by stair, if stair is filled put one chip on the wall and clean the extra tiles on the stair
build_and_clean(_, []) :- !.
build_and_clean(ID, [(Stair, Color)|Stairs]) :-
    set_value_wall(ID, Stair, _, Color, 1),
    clean_stair(ID, Stair, Color),
    build_and_clean(ID, Stairs).

%Get the most left position of a stair that is colored ( means it is complete) and updates the wall and stairs
build_wall(ID) :-
    setof((Stair, Color),
          (stair(ID, Stair, Stair, Color), Color=\=0),
          Stairs),
    build_and_clean(ID, Stairs).
