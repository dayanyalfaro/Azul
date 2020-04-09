:- consult(board).
:- consult(environment).
:- consult(strategy).
:- consult(punctuation).

%-----------------Player Initialization--------------------
init_player(ID) :-
    set_score(ID, 0),
    init_board(ID).

create_players(0):-!.
create_players(Cant) :- init_player(Cant), Id is Cant - 1, create_players(Id). 

%-----------------Player Actions---------------------------

%-----(Fase 1)----Player action: pick a movement and execute it 

%Update center after player takes tiles from it
update_environment(0,Color,1):-!, remove_tiles_center(Color,_), remove_chip_center().
update_environment(0,Color,0):-!, remove_tiles_center(Color,_).
%Move the rest of tiles to the table center after the player take the selected tiles from factory Source
update_environment(Source, Color, _) :-
    remove_tiles_factory(Source, Color, _),
    remove_tiles_factory(Source, 1, B),
    remove_tiles_factory(Source, 2, Y),
    remove_tiles_factory(Source, 3, R),
    remove_tiles_factory(Source, 4, G),
    remove_tiles_factory(Source, 5, W),
    add_tile_center(1, B),
    add_tile_center(2, Y),
    add_tile_center(3, R),
    add_tile_center(4, G),
    add_tile_center(5, W).


%Place tile by tile on the lid
update_lid(Amount, _) :- Amount =< 0,!. 
update_lid(Amount, Color) :-
    Amount > 0,
    add_tile_lid(Color),
    K is Amount-1,
    update_lid(K, Color).

%Put the fist player chip in the floor if it was taken 
%TODO set player ID as first_player
place_chip(_, 0) :- !.
place_chip(ID, 1) :- set_first_player(ID),
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
    Extra=<0,!.                        
place_extras(ID, Color, Extra) :-  
    Extra>0,
    findall(P, floor(ID, P, 0, _), Unset),
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
    printText('\n Turn of Player ', red), printText(ID, red), printText(' -> ', red),
    get_moves(ID,All_moves),
    strategy(ID,All_moves,Source, Color, Amount, Stair, Chip),
    printPick([Source,Color,Amount,Stair,Chip]), %TODO: modificar para que diga la accion que va a hacer el jugador
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
    set_score(ID, Score).

floor_to_lid([]):-!.
floor_to_lid([C|L]):- add_tile_lid(C),floor_to_lid(L).

clean_floor(ID) :-
    findall(C,
            ( floor(ID, _, C, _),
              C=\=0,
              C =\= -1
            ),
            Colors),
    floor_to_lid(Colors).
            

%Get the most left position of a stair that is colored ( means it is complete) and updates the wall and stairs
build_wall(ID) :-
    ((setof((Stair, Color),
          (stair(ID, Stair, Stair, Color), Color=\=0),
          Stairs),
    build_and_clean(ID, Stairs)),!;true),
    floor_penalty(ID),
    clean_floor(ID).


update_all_walls(0):- !.
update_all_walls(Cant):- build_wall(Cant), ID is Cant - 1, update_all_walls(ID).



