:- consult(matrix).
:- consult(utils).

:- dynamic(cant_players/1, first_player/1, next_turn/1, stop_play/1, cant_factory/1, factory/6, center/6, bag/2, lid/2).

%------------------------Play ATTR----------------------------------------
set_cant_players(Cant) :-
    assert(cant_players(Cant)). 

set_first_player(ID) :- (retract(first_player(_)), ! ; true), assert(first_player(ID)).

set_next_turn(Next):- (retract(next_turn(_)), ! ; true),  assert(next_turn(Next)).

plus_one(X, 1) :- cant_players(C), X =:= C, !. 
plus_one(X, Y) :- Y is X + 1.

update_next_turn() :- next_turn(Actual),  plus_one(Actual, Next) , set_next_turn(Next).

set_stop_play(Bool):-  (retract(stop_play(_)), ! ; true),  assert(stop_play(Bool)).

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


%------------------------Bag of tiles----------------------------------------
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

fill_bag() :-
    bag(N1, B),
    retract(bag(N1, B)),
    lid(N2, L),
    retract(lid(N2, L)),
    concat(B, L, R),
    N3 is N1+N2,
    assert(bag(N3, R)),
    assert(lid(0, [])). %fill the bag with all tiles in lid, and then lid is empty

%-----------------------Lid of tiles---------------------------------------------
init_lid() :-
    assert(lid(0, [])).
add_tile_lid(C) :-
    lid(N, L),
    retract(lid(N, L)),
    N1 is N+1,
    assert(lid(N1, [C|L])). %add a tile of color C to the lid
remove_tile_lid(C) :-
    lid(N, L),
    remove_first(L, L1, C),
    (   C=\= -1, !
    ;   true
    ),
    retract(lid(N, L)),
    N1 is N-1,
    N2 is max(0, N1),
    assert(lid(N2, L1)). %delete the first tile in lid and return its color C. If the lid is empty return -1

%-----------------------Factory-------------------------------------------------------
add_B_factory(ID) :-
    factory(ID, B, Y, R, G, W),
    B1 is B+1,
    set_factory(ID, B1, Y, R, G, W).
add_Y_factory(ID) :-
    factory(ID, B, Y, R, G, W),
    Y1 is Y+1,
    set_factory(ID, B, Y1, R, G, W).
add_R_factory(ID) :-
    factory(ID, B, Y, R, G, W),
    R1 is R+1,
    set_factory(ID, B, Y, R1, G, W).
add_G_factory(ID) :-
    factory(ID, B, Y, R, G, W),
    G1 is G+1,
    set_factory(ID, B, Y, R, G1, W).
add_W_factory(ID) :-
    factory(ID, B, Y, R, G, W),
    W1 is W+1,
    set_factory(ID, B, Y, R, G, W1).

% add a tile from the bag to factory number ID
add_tile_factory(ID) :-
    remove_tile_bag(C),
    (   C=:=1, !
    ->  add_B_factory(ID)
    ;   true
    ),
    (   C=:=2, !
    ->  add_Y_factory(ID)
    ;   true
    ),
    (   C=:=3, !
    ->  add_R_factory(ID)
    ;   true
    ),
    (   C=:=4, !
    ->  add_G_factory(ID)
    ;   true
    ),
    (   C=:=5, !
    ->  add_W_factory(ID)
    ;   true
    ).

% initialize factory ID with 4 tiles. If the bag is empty no tile is added.
init_factory(ID) :-
    set_factory(ID, 0, 0, 0, 0, 0),
    add_tile_factory(ID),
    add_tile_factory(ID),
    add_tile_factory(ID),
    add_tile_factory(ID).

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

remove_B_factory(ID, B) :-
    factory(ID, B, Y, R, G, W),
    set_factory(ID, 0, Y, R, G, W).
remove_Y_factory(ID, Y) :-
    factory(ID, B, Y, R, G, W),
    set_factory(ID, B, 0, R, G, W).
remove_R_factory(ID, R) :-
    factory(ID, B, Y, R, G, W),
    set_factory(ID, B, Y, 0, G, W).
remove_G_factory(ID, G) :-
    factory(ID, B, Y, R, G, W),
    set_factory(ID, B, Y, R, 0, W).
remove_W_factory(ID, W) :-
    factory(ID, B, Y, R, G, W),
    set_factory(ID, B, Y, R, G, 0).

%remove all tiles of color C from the factory ID and return in Cant the number of this tiles eliminated
remove_tiles_factory(ID, C, Cant) :-
    (   C=:=1, !
    ->  remove_B_factory(ID, Cant)
    ;   true
    ),
    (   C=:=2, !
    ->  remove_Y_factory(ID, Cant)
    ;   true
    ),
    (   C=:=3, !
    ->  remove_R_factory(ID, Cant)
    ;   true
    ),
    (   C=:=4, !
    ->  remove_G_factory(ID, Cant)
    ;   true
    ),
    (   C=:=5, !
    ->  remove_W_factory(ID, Cant)
    ;   true
    ).

empty_factory(ID):- factory(ID, B, Y, R, G, W), B =:= 0, Y =:= 0, R =:= 0, G =:= 0, W =:= 0. 

empty_all_factory():-  findall(ID, empty_factory(ID), L), cant_factory(Cant), length(L, Cant).




%------------------------Center of the Table-------------------------------------------------
add_B_center(K) :-
    center(T1, B, Y, R, G, W),
    B1 is B+K,
    set_center(T1, B1, Y, R, G, W).
add_Y_center(K) :-
    center(T1, B, Y, R, G, W),
    Y1 is Y+K,
    set_center(T1, B, Y1, R, G, W).
add_R_center(K) :-
    center(T1, B, Y, R, G, W),
    R1 is R+K,
    set_center(T1, B, Y, R1, G, W).
add_G_center(K) :-
    center(T1, B, Y, R, G, W),
    G1 is G+K,
    set_center(T1, B, Y, R, G1, W).
add_W_center(K) :-
    center(T1, B, Y, R, G, W),
    W1 is W+K,
    set_center(T1, B, Y, R, G, W1).

init_center() :-
    set_center(1, 0, 0, 0, 0, 0).

%add K tiles of color C to the center
add_tile_center(C, K) :-
    (   C=:=1, !
    ->  add_B_center(K)
    ;   true
    ),
    (   C=:=2, !
    ->  add_Y_center(K)
    ;   true
    ),
    (   C=:=3, !
    ->  add_R_center(K)
    ;   true
    ),
    (   C=:=4, !
    ->  add_G_center(K)
    ;   true
    ),
    (   C=:=5, !
    ->  add_W_center(K)
    ;   true
    ).

remove_B_center(B) :-
    center(T1, B, Y, R, G, W),
    set_center(T1, 0, Y, R, G, W).
remove_Y_center(Y) :-
    center(T1, B, Y, R, G, W),
    set_center(T1, B, 0, R, G, W).
remove_R_center(R) :-
    center(T1, B, Y, R, G, W),
    set_center(T1, B, Y, 0, G, W).
remove_G_center(G) :-
    center(T1, B, Y, R, G, W),
    set_center(T1, B, Y, R, 0, W).
remove_W_center(W) :-
    center(T1, B, Y, R, G, W),
    set_center(T1, B, Y, R, G, 0).
remove_chip_center():- center(1, B, Y, R, G, W),set_center(0, B, Y, R, G, W).
%remove all tiles of color C from the center and return in Cant the number of this tiles eliminated
remove_tiles_center(C, Cant):- ((C =:= 1, !) -> remove_B_center(Cant); true), ((C =:= 2, !) -> remove_Y_center(Cant); true), ((C =:= 3, !) -> remove_R_center(Cant); true), ((C =:= 4, !) -> remove_G_center(Cant); true), ((C =:= 5, !) -> remove_W_center(Cant);  true).

empty_center():- center(_, B, Y, R, G, W), B =:= 0, Y =:= 0, R =:= 0, G =:= 0, W =:= 0. 


%------------------------Play Initialization-------------------------------------------------

init_play(CantPlayers):- set_cant_players(CantPlayers), set_first_player(1), set_next_turn(1), init_bag(), init_lid(), init_all_factorys(), init_center().

%------------------------FASES Play-----------------------------------------------------------

%Check if all factorys and the center is empty(End FASE 1)
finish_fase1() :- empty_all_factory(), empty_center().

init_all_floor(1) :- init_floor(1), ! . 
init_all_floor(Cant) :- init_floor(Cant), Id is Cant - 1, init_all_floor(Id).

restart_all_floor() :- retractall(floor), cant_players(Cant), init_all_floor(Cant).

prepare_next_round():-fill_bag(), bag(Cant, _), Cant =\= 0, init_all_factorys(), init_center(), first_player(P), set_next_turn(P), restart_all_floor().




%------------------------Print PLay State------------------------------------------------------

print_factory(ID):- factory(ID, B, Y, R, G, W), printB('Factory '), printB(ID), printB(': '), printB(B), printY(Y), printR(R), printG(G), printW(W).

print_all_factory(0):- write('\n'), !.
print_all_factory(ID):- print_factory(ID), write('    '), ID1 is ID - 1, print_all_factory(ID1).

print_bag():- write('\n'), bag(S, B), printB('BAG->  Size: '), write(S), printB('  Tiles: '), write(B), write('\n').
print_lid():- lid(N, L), printB('LID->  Size: '), write(N), printB('  Tiles: '), write(L), write('\n').
print_center():- center(T, B, Y, R, G, W), printB('Center-> '), printB(B), printY(Y),  printR(R), printG(G), printW(W), ((T =:= 1) -> printW(' |1|');true).

print_all_boards(1):- print_board(1), !.
print_all_boards(Cant):- print_board(Cant), ID is Cant - 1, print_all_boards(ID).

print_play_state():- cant_factory(F), print_all_factory(F), print_center(), print_bag(), print_lid(), cant_players(C),  print_all_boards(C).