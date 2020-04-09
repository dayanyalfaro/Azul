:- consult(matrix).
:- consult(utils).

:- dynamic(cant_players/1,
           first_player/1,
           next_turn/1,
           stop_play/1,
           cant_factory/1,
           factory/6,
           center/6,
           bag/2,
           lid/2).

%------------------------Play ATTR--------------------------------------------------------------------------------------------
set_cant_players(Cant) :-
    assert(cant_players(Cant)). 

set_first_player(ID) :-
    (   retract(first_player(_)), !
    ;   true
    ),
    assert(first_player(ID)).

set_next_turn(Next) :-
    (   retract(next_turn(_)), !
    ;   true
    ),
    assert(next_turn(Next)).

plus_one(X, 1) :-
    cant_players(C),
    X=:=C, !. 
plus_one(X, Y) :-
    Y is X+1.

update_next_turn() :-
    next_turn(Actual),
    plus_one(Actual, Next),
    set_next_turn(Next).

set_stop_play(Bool) :-
    (   retract(stop_play(_)), !
    ;   true
    ),
    assert(stop_play(Bool)).

set_cant_factory(Cant) :-
    (   retract(cant_factory(_)), !
    ;   true
    ),
    assert(cant_factory(Cant)).

set_factory(ID, B, Y, R, G, W) :-
    (   retract(factory(ID,
                        _,
                        _,
                        _,
                        _,
                        _)), !
    ;   true
    ),
    assert(factory(ID,
                   B,
                   Y,
                   R,
                   G,
                   W)).

set_center(Tile1, B, Y, R, G, W) :-
    (   retract(center(_,
                       _,
                       _,
                       _,
                       _,
                       _)), !
    ;   true
    ),
    assert(center(Tile1, B, Y, R, G, W)).

%------------------------Bag of tiles---------------------------------------------------------------------------------------
create_bag(0, 0, 0, 0, 0, []) :- !.
create_bag(0, 0, 0, 0, W, [5|T]) :- !,
    W1 is W-1,
    create_bag(0, 0, 0, 0, W1, T).
create_bag(0, 0, 0, G, W, [4|T]) :- !,
    G1 is G-1,
    create_bag(0, 0, 0, G1, W, T).
create_bag(0, 0, R, G, W, [3|T]) :- !,
    R1 is R-1,
    create_bag(0, 0, R1, G, W, T).
create_bag(0, Y, R, G, W, [2|T]) :- !,
    Y1 is Y-1,
    create_bag(0, Y1, R, G, W, T).
create_bag(B, Y, R, G, W, [1|T]) :-
    B1 is B-1,
    create_bag(B1, Y, R, G, W, T). 

init_bag() :-
    create_bag(20, 20, 20, 20, 20, L),
    assert(bag(100, L)). %init the bag with 20 tiles of each color
add_tile_bag(C) :-
    bag(N, L),
    retract(bag(N, L)),
    N1 is N+1,
    assert(bag(N1, [C|L])). %add a tile of color C to the bag
remove_tile_bag(-1) :-
    bag(N, _),
    N=:=0, !. % if the bag is empty => return color -1
remove_tile_bag(C) :-
    bag(N, L),
    N2 is N-1,
    N1 is N+1,
    random(1, N1, I),
    retract(bag(N, L)),
    remove(L, I, R, C),
    assert(bag(N2, R)).

%fill the bag with all tiles in lid, and then lid is empty
fill_bag() :-
    bag(N1, B),
    retract(bag(N1, B)),
    lid(N2, L),
    retract(lid(N2, L)),
    concat(B, L, R),
    N3 is N1+N2,
    assert(bag(N3, R)),
    assert(lid(0, [])). 

%-----------------------Lid of tiles--------------------------------------------------------------------------------------------------
init_lid() :-
    assert(lid(0, [])).

% add a tile of color C to the lid
add_tile_lid(C) :-
    lid(N, L),
    retract(lid(N, L)),
    N1 is N+1,
    assert(lid(N1, [C|L])). 

%delete the first tile in lid and return its color C. If the lid is empty return -1    
remove_tile_lid(C) :-
    lid(N, L),
    remove_first(L, L1, C),
    (   C=\= -1, !
    ;   true
    ),
    retract(lid(N, L)),
    N1 is N-1,
    N2 is max(0, N1),
    assert(lid(N2, L1)). 

%-----------------------Factory-----------------------------------------------------------------------------------------------------------


% add a tile to factory number ID
add_tile_factory(_, -1) :- !.
add_tile_factory(ID, 1) :- !,
    factory(ID, B, Y, R, G, W),
    B1 is B+1,
    set_factory(ID, B1, Y, R, G, W).
add_tile_factory(ID, 2) :- !,
    factory(ID, B, Y, R, G, W),
    Y1 is Y+1,
    set_factory(ID, B, Y1, R, G, W).
add_tile_factory(ID, 3) :- !,
    factory(ID, B, Y, R, G, W),
    R1 is R+1,
    set_factory(ID, B, Y, R1, G, W).
add_tile_factory(ID, 4) :- !,
    factory(ID, B, Y, R, G, W),
    G1 is G+1,
    set_factory(ID, B, Y, R, G1, W).
add_tile_factory(ID, 5) :- !,
    factory(ID, B, Y, R, G, W),
    W1 is W+1,
    set_factory(ID, B, Y, R, G, W1).

% initialize factory ID with 4 tiles. If the bag is empty no tile is added.
init_factory(ID) :-
    set_factory(ID, 0, 0, 0, 0, 0),
    remove_tile_bag(C1),
    add_tile_factory(ID, C1),
    remove_tile_bag(C2),
    add_tile_factory(ID, C2),
    remove_tile_bag(C3),
    add_tile_factory(ID, C3),
    remove_tile_bag(C4),
    add_tile_factory(ID, C4).

% P: cant of players of the game, create 2 factorys per player + 1 factory.
create_factory(P) :-
    P=:=0, !,
    cant_factory(C),
    ID is C+1,
    init_factory(ID),
    set_cant_factory(ID).
create_factory(P) :-
    cant_factory(C),
    ID1 is C+1,
    init_factory(ID1),
    ID2 is ID1+1,
    init_factory(ID2),
    set_cant_factory(ID2),
    P1 is P-1,
    create_factory(P1).

%initialize all factory with new tiles from the bag
init_all_factorys() :-
    set_cant_factory(0),
    cant_players(P),
    create_factory(P).

%remove all tiles of color a specific from the factory ID and return in Cant the number of this tiles eliminated
remove_tiles_factory(ID, 1, Cant) :- !,
    factory(ID, Cant, Y, R, G, W),
    set_factory(ID, 0, Y, R, G, W).
remove_tiles_factory(ID, 2, Cant) :- !,
    factory(ID, B, Cant, R, G, W),
    set_factory(ID, B, 0, R, G, W).
remove_tiles_factory(ID, 3, Cant) :- !,
    factory(ID, B, Y, Cant, G, W),
    set_factory(ID, B, Y, 0, G, W).
remove_tiles_factory(ID, 4, Cant) :- !,
    factory(ID, B, Y, R, Cant, W),
    set_factory(ID, B, Y, R, 0, W).
remove_tiles_factory(ID, 5, Cant) :- !,
    factory(ID, B, Y, R, G, Cant),
    set_factory(ID, B, Y, R, G, 0).

%Check if a the ID factory is empty
empty_factory(ID) :-
    factory(ID, B, Y, R, G, W),
    B=:=0,
    Y=:=0,
    R=:=0,
    G=:=0,
    W=:=0. 

%Check if all factory are empty
empty_all_factory() :-
    findall(ID, empty_factory(ID), L),
    cant_factory(Cant),
    length(L, Cant).


%------------------------Center of the Table-------------------------------------------------
init_center() :-
    set_center(1, 0, 0, 0, 0, 0).

%add K tiles of color C to the center
add_tile_center(1, K) :- !,
    center(T1, B, Y, R, G, W),
    B1 is B+K,
    set_center(T1, B1, Y, R, G, W).
add_tile_center(2, K) :- !,
    center(T1, B, Y, R, G, W),
    Y1 is Y+K,
    set_center(T1, B, Y1, R, G, W).
add_tile_center(3, K) :- !,
    center(T1, B, Y, R, G, W),
    R1 is R+K,
    set_center(T1, B, Y, R1, G, W).
add_tile_center(4, K) :- !,
    center(T1, B, Y, R, G, W),
    G1 is G+K,
    set_center(T1, B, Y, R, G1, W).
add_tile_center(5, K) :- !,
    center(T1, B, Y, R, G, W),
    W1 is W+K,
    set_center(T1, B, Y, R, G, W1).

%remove the initial chip from the center
remove_chip_center() :-
    center(1, B, Y, R, G, W),
    set_center(0, B, Y, R, G, W).

%remove all tiles of color C from the center and return in Cant the number of this tiles eliminated
remove_tiles_center(1, Cant) :- !,
    center(T1, Cant, Y, R, G, W),
    set_center(T1, 0, Y, R, G, W).
remove_tiles_center(2, Cant) :- !,
    center(T1, B, Cant, R, G, W),
    set_center(T1, B, 0, R, G, W).
remove_tiles_center(3, Cant) :- !,
    center(T1, B, Y, Cant, G, W),
    set_center(T1, B, Y, 0, G, W).
remove_tiles_center(4, Cant) :- !,
    center(T1, B, Y, R, Cant, W),
    set_center(T1, B, Y, R, 0, W).
remove_tiles_center(5, Cant) :- !,  center(T1, B, Y, R, G, Cant), set_center(T1, B, Y, R, G, 0).


%check if the center is empty
empty_center():- center(_, B, Y, R, G, W), B =:= 0, Y =:= 0, R =:= 0, G =:= 0, W =:= 0. 


%------------------------Play Initialization-------------------------------------------------------------------------------

init_play(CantPlayers):- set_cant_players(CantPlayers), set_first_player(1), set_next_turn(1), init_bag(), init_lid(), init_all_factorys(), init_center().

%------------------------FASES Actions---------------------------------------------------------------------------------------

%Check if all factorys and the center is empty(End FASE 1)
finish_fase1() :- empty_all_factory(), empty_center().

init_all_floor(1) :- init_floor(1), ! . 
init_all_floor(Cant) :- init_floor(Cant), Id is Cant - 1, init_all_floor(Id).

restart_all_floor() :- retractall(floor(_,_,_,_)), cant_players(Cant), init_all_floor(Cant).

prepare_next_round() :-  bag(Cant, _), (Cant =\= 0; fill_bag()),bag(Cant1, _),Cant1 =\= 0, init_all_factorys(), init_center(), first_player(P), set_next_turn(P), restart_all_floor().



%------------------------Print PLay State------------------------------------------------------------------------------------

print_factory(ID):- factory(ID, B, Y, R, G, W), printText('Factory ', blue), printText(ID, blue), printText(': ', blue), printText(B, blue), printText(Y, yellow), printText(R, red), printText(G, green), printText(W, white), !.

print_all_factory(Cant, ID):- ID > Cant, !, write('\n'), !.
print_all_factory(Cant, ID):- print_factory(ID), write('    '), ID1 is ID + 1, print_all_factory(Cant, ID1).

print_bag():- write('\n'), bag(S, B), printText('BAG->  Size: ', blue), write(S), printText('  Tiles: ', blue), write(B), write('\n').
print_lid():- lid(N, L), printText('LID->  Size: ', blue), write(N), printText('  Tiles: ', blue), write(L), write('\n').
print_center():- center(T, B, Y, R, G, W), printText('Center-> ', blue), printText(B, blue), printText(Y, yellow),  printText(R, red), printText(G, green), printText(W, white), ((T =:= 1) -> printText(' |1|', white);true).

print_all_boards(Cant, ID):- ID > Cant, !.
print_all_boards(Cant, ID):- print_board(ID), ID2 is ID + 1, print_all_boards(Cant, ID2).

print_table():- cant_factory(F), print_all_factory(F, 1), print_center(), print_bag(), print_lid().

print_play_state():- cant_factory(F), print_all_factory(F, 1), print_center(), print_bag(), print_lid(), cant_players(C),  print_all_boards(C, 1).
