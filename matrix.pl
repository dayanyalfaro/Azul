:- consult(utils).
:- dynamic(cell/5, stair/4, player_score/2, floor/4).



%-----------------Matrix methods
%Color Definition
% 1 - Blue
% 2- Yellow
% 3 - Red 
% 4 - Green
% 5 - White
init_wall([[1, 2, 3, 4, 5], [5, 1, 2, 3, 4], [4, 5, 1, 2, 3], [3, 4, 5, 1, 2], [2, 3, 4, 5, 1]]).

init_matrix_wall_col(ID, I, [A, B, C, D, E]) :-
    assert(cell(ID, I, 1, A, 0)),
    assert(cell(ID, I, 2, B, 0)),
    assert(cell(ID, I, 3, C, 0)),
    assert(cell(ID, I, 4, D, 0)),
    assert(cell(ID, I, 5, E, 0)).

init_matrix_wall(ID) :-
    init_wall([A, B, C, D, E]),
    init_matrix_wall_col(ID, 1, A),
    init_matrix_wall_col(ID, 2, B),
    init_matrix_wall_col(ID, 3, C),
    init_matrix_wall_col(ID, 4, D),
    init_matrix_wall_col(ID, 5, E). 

%Change the cell state (I, J) in the wall
set_value_wall(ID, I, J, C, V) :-
    retract(cell(ID, I, J, C, _)),
    assert(cell(ID, I, J, C, V)).


init_stair(ID) :-
    assert(stair(ID, 1, 1, 0)),
    assert(stair(ID, 2, 1, 0)),
    assert(stair(ID, 2, 2, 0)),
    assert(stair(ID, 3, 1, 0)),
    assert(stair(ID, 3, 2, 0)),
    assert(stair(ID, 3, 3, 0)),
    assert(stair(ID, 4, 1, 0)),
    assert(stair(ID, 4, 2, 0)),
    assert(stair(ID, 4, 3, 0)),
    assert(stair(ID, 4, 4, 0)),
    assert(stair(ID, 5, 1, 0)),
    assert(stair(ID, 5, 2, 0)),
    assert(stair(ID, 5, 3, 0)),
    assert(stair(ID, 5, 4, 0)),
    assert(stair(ID, 5, 5, 0)).

%Change the cell state (I, J) in the stair
set_value_stair(ID, I, J, Color) :-
    retract(stair(ID, I, J, _)),
    assert(stair(ID, I, J, Color)).

init_floor(ID) :-
    assert(floor(ID, 1, 0, -1)),
    assert(floor(ID, 2, 0, -1)),
    assert(floor(ID, 3, 0, -2)),
    assert(floor(ID, 4, 0, -2)),
    assert(floor(ID, 5, 0, -2)),
    assert(floor(ID, 6, 0, -3)),
    assert(floor(ID, 7, 0, -3)).


set_value_floor(ID, Pos, Value) :-
    retract(floor(ID, Pos, _, Penalty)),
    assert(floor(ID, Pos, Value, Penalty)).

%Initialize the board of the player ID
init_board(ID) :-
    init_matrix_wall(ID),
    init_floor(ID),
    init_stair(ID).

 %---------------------Print Board----------------------------


print_floor_row(ID):- write('|'), floor(ID, 1, C1, _), floor(ID, 2, C2, _), floor(ID, 3, C3, _),floor(ID, 4, C4, _),floor(ID, 5, C5, _), floor(ID, 6, C6, _),floor(ID, 7, C7, _), print_cell(C1, 1), print_cell(C2, 1), print_cell(C3, 1), print_cell(C4, 1), print_cell(C5, 1), print_cell(C6, 1),  print_cell(C7, 1), write('                 |\n'), !.
print_floor(ID):- !,  write('|-1  -1  -2  -2  -2  -3  -3                   |\n'), print_floor_row(ID).

print_line():-  write('-----------------------------------------------\n').

print_title(ID):- printText('\n Player:', blue), printText(ID, blue), printText('   Score:', red), player_score(ID, S), printText(S, red),  write('\n').

print_board(ID):-  print_title(ID), print_line(), print_board_row(ID, 1) , print_board_row(ID, 2), print_board_row(ID, 3), print_board_row(ID, 4), print_board_row(ID, 5), print_floor(ID), print_line().
