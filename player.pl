:- consult(matrix).
:- consult(strategy).

:- dynamic(player_score/2, player_wall/2, player_stair/2, player_penalty/2).

%-----------------Player ATTR----------------------------
set_score(ID, SCORE) :- ( (retract(player_score(ID, _)), !) ; true ) , assert(player_score(ID, SCORE)).
set_penalty(ID, PENALTY):- ( (retract(player_penalty(ID, _)), !) ; true ) , assert(player_penalty(ID, PENALTY)).

%-----------------Player Initialization--------------------
init_player(ID):- set_score(ID, 0), init_board(ID), set_penalty(ID, 0). 

%-----------------Player Actions---------------------------

%-----------------Player Actions

%-----(Fase 1)----Player action: pick a movement and execute it 
update_environment(_, _, _).

%Put the fist player chip in the floor if it was taken 
place_chip(_, 0) :- !.
place_chip(ID, 1) :-
    place_extras(ID, -1, 1).

%Fill the floor
update_floor(_, _, _, 0) :- !. 
update_floor(_, [], _, _) :- !. 
update_floor(ID, [P|Unset], Color, Amount) :-
    set_value_floor(ID, P, Color),
    K is Amount-1,
    update_floor(ID, Unset, Color, K).

%Place the extra chips of the stair on the floor
place_extras(_, _, Extra) :-
    Extra=<0, !.
place_extras(ID, Color, Extra) :-
    Extra>0,
    setof(P, floor(ID, P, 0), Unset),
    update_floor(ID, Unset, Color, Extra).
    % length(Unset, L),
    % Garbage is Extra-L,
    % update_lid()

%Place color chips one by one on a stair
update_stair(_, _, _, _, 0) :- !.
update_stair(_, _, [], _, _) :- !.
update_stair(ID, Stair, [P|Unset], Color, Amount) :-
    set_value_stair(ID, Stair, P, Color),
    K is Amount-1,
    update_stair(ID, Stair, Unset, Color, K).

%Fill a stair according to the amount and color of the taken chips and place the extra chips on the floor
place_colors(ID, Stair, Color, Amount) :-
    setof(P, stair(ID, Stair, P, 0), Unset),
    update_stair(ID, Stair, Unset, Color, Amount),
    length(Unset, L),
    Extra is Amount-L,
    place_extras(ID, Color, Extra).

%Pick a movement and execute it 
pick(ID) :-
    strategy(Source, Color, Amount, Stair, Chip),
    update_environment(Source, Color, Amount),
    place_chip(ID, Chip),
    place_colors(ID, Stair, Color, Amount).

%-----(Fase 2)----Player action: build the wall

%Fill the stair with 0 and put the extra chips on the lid
clean_stair(ID, Stair, Color) :-
    setof(P, stair(ID, Stair, P, Color), Unset),
    update_stair(ID, Stair, Unset, 0, Stair).
    %Gargabe is Stair-1,
    %update_lid() 

%Goes stair by stair, if stair is filled put one chip on the wall and clean the extra chips on the stair
build_and_clean(_, []) :- !.
build_and_clean(ID, [(Stair, Color)|Stairs]) :-
    set_value_wall(ID, Stair, _, Color, 1),
    clean_stair(ID, Stair, Color),
    build_and_clean(ID, Stairs).

%Get the most left position of a stair that is colored ( means it is complete) and update the wall and stairs
build_wall(ID) :-
    setof((Stair, Color),
          (stair(ID, Stair, Stair, Color), Color=\=0),
          Stairs),
    build_and_clean(ID, Stairs).
