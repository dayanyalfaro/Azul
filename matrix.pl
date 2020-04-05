:- consult(utils).
:- dynamic(cell/5, stair/4, floor/4).



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

init_board(ID) :-
    init_matrix_wall(ID),
    init_floor(ID),
    init_stair(ID).

 %---------------------Print Board----------------------------
 % Print the wall cells
print_cell(0, 1):- !, ansi_format([bold, fg(black)], '(_) ', []).  %is empty the floor cell
print_cell(-1, 1):- !,  ansi_format([bold, fg(white)], '(1) ', []). %is chip 1 in the floor cell
print_cell(1, 0) :- !,
    ansi_format([bold, fg(blue)], '(_) ', []).
print_cell(1, 1) :- !,
    ansi_format([bold, fg(blue)], '(X) ', []).
print_cell(2, 0) :- !,
    ansi_format([bold, fg(yellow)], '(_) ', []).
print_cell(2, 1) :- !,
    ansi_format([bold, fg(yellow)], '(X) ', []).
print_cell(3, 0) :- !,
    ansi_format([bold, fg(red)], '(_) ', []).
print_cell(3, 1) :- !,
    ansi_format([bold, fg(red)], '(X) ', []).
print_cell(4, 0) :- !,
    ansi_format([bold, fg(green)], '(_) ', []).
print_cell(4, 1) :- !,
    ansi_format([bold, fg(green)], '(X) ', []).
print_cell(5, 0) :- !,
    ansi_format([bold, fg(white)], '(_) ', []).
print_cell(5, 1) :- !,
    ansi_format([bold, fg(white)], '(X) ', []).

% Print the stair cells
print_stair(0) :-
    ansi_format([bold, fg(cyan)], '(_) ', []).
print_stair(1) :-
    ansi_format([bold, fg(blue)], '(X) ', []).
print_stair(2) :-
    ansi_format([bold, fg(yellow)], '(X) ', []).
print_stair(3) :-
    ansi_format([bold, fg(red)], '(X) ', []).
print_stair(4) :-
    ansi_format([bold, fg(green)], '(X) ', []).
print_stair(5) :-
    ansi_format([bold, fg(white)], '(X) ', []).

% Print the stair rows
print_stair_row(ID, 1) :- !,
    write('|                '),
    stair(ID, 1, 1, S),
    print_stair(S),
    write('  |  ').
print_stair_row(ID, 2) :- !,
    write('|            '),
    stair(ID, 2, 2, S1),
    print_stair(S1),
    stair(ID, 2, 1, S2),
    print_stair(S2),
    write('  |  ').
print_stair_row(ID, 3) :- !,
    write('|        '),
    stair(ID, 3, 3, S1),
    print_stair(S1),
    stair(ID, 3, 2, S2),
    print_stair(S2),
    stair(ID, 3, 1, S3),
    print_stair(S3),
    write('  |  ').
print_stair_row(ID, 4) :- !,
    write('|    '),
    stair(ID, 4, 4, S1),
    print_stair(S1),
    stair(ID, 4, 3, S2),
    print_stair(S2),
    stair(ID, 4, 2, S3),
    print_stair(S3),
    stair(ID, 4, 1, S4),
    print_stair(S4),
    write('  |  ').
print_stair_row(ID, 5) :-
    write('|'),
    stair(ID, 5, 5, S1),
    print_stair(S1),
    stair(ID, 5, 4, S2),
    print_stair(S2),
    stair(ID, 5, 3, S3),
    print_stair(S3),
    stair(ID, 5, 2, S4),
    print_stair(S4),
    stair(ID, 5, 1, S5),
    print_stair(S5),
    write('  |  ').

% Print the wall rows
print_wall_row(ID, 1) :- !,
    cell(ID, 1, 1, C1, S1),
    print_cell(C1, S1),
    cell(ID, 1, 2, C2, S2),
    print_cell(C2, S2),
    cell(ID, 1, 3, C3, S3),
    print_cell(C3, S3),
    cell(ID, 1, 4, C4, S4),
    print_cell(C4, S4),
    cell(ID, 1, 5, C5, S5),
    print_cell(C5, S5).
print_wall_row(ID, 2) :- !,
    cell(ID, 2, 1, C1, S1),
    print_cell(C1, S1),
    cell(ID, 2, 2, C2, S2),
    print_cell(C2, S2),
    cell(ID, 2, 3, C3, S3),
    print_cell(C3, S3),
    cell(ID, 2, 4, C4, S4),
    print_cell(C4, S4),
    cell(ID, 2, 5, C5, S5),
    print_cell(C5, S5).
print_wall_row(ID, 3) :- !,
    cell(ID, 3, 1, C1, S1),
    print_cell(C1, S1),
    cell(ID, 3, 2, C2, S2),
    print_cell(C2, S2),
    cell(ID, 3, 3, C3, S3),
    print_cell(C3, S3),
    cell(ID, 3, 4, C4, S4),
    print_cell(C4, S4),
    cell(ID, 3, 5, C5, S5),
    print_cell(C5, S5).
print_wall_row(ID, 4) :- !,
    cell(ID, 4, 1, C1, S1),
    print_cell(C1, S1),
    cell(ID, 4, 2, C2, S2),
    print_cell(C2, S2),
    cell(ID, 4, 3, C3, S3),
    print_cell(C3, S3),
    cell(ID, 4, 4, C4, S4),
    print_cell(C4, S4),
    cell(ID, 4, 5, C5, S5),
    print_cell(C5, S5).
print_wall_row(ID, 5) :- !,
    cell(ID, 5, 1, C1, S1),
    print_cell(C1, S1),
    cell(ID, 5, 2, C2, S2),
    print_cell(C2, S2),
    cell(ID, 5, 3, C3, S3),
    print_cell(C3, S3),
    cell(ID, 5, 4, C4, S4),
    print_cell(C4, S4),
    cell(ID, 5, 5, C5, S5),
    print_cell(C5, S5).

%Print the board rows
print_board_row(ID, 1) :- !,
    print_stair_row(ID, 1),
    print_wall_row(ID, 1),
    write('| \n').
print_board_row(ID, 2) :- !,
    print_stair_row(ID, 2),
    print_wall_row(ID, 2),
    write('| \n').
print_board_row(ID, 3) :- !,
    print_stair_row(ID, 3),
    print_wall_row(ID, 3),
    write('| \n').
print_board_row(ID, 4) :- !,
    print_stair_row(ID, 4),
    print_wall_row(ID, 4),
    write('| \n').
print_board_row(ID, 5):- print_stair_row(ID, 5), print_wall_row(ID, 5), write('| \n').

print_floor_row(ID):- write('|'), floor(ID, 1, C1, _), floor(ID, 2, C2, _), floor(ID, 3, C3, _),floor(ID, 4, C4, _),floor(ID, 5, C5, _), floor(ID, 6, C6, _),floor(ID, 7, C7, _), print_cell(C1, 1), print_cell(C2, 1), print_cell(C3, 1), print_cell(C4, 1), print_cell(C5, 1), print_cell(C6, 1),  print_cell(C7, 1), write('                 |\n').
print_floor(ID):- !,  write('|-1  -1  -2  -2  -2  -3  -3                   |\n'), print_floor_row(ID).

print_line():-  write('-----------------------------------------------\n').



print_board(ID):- !,printB('\n Player:'), printB(ID), write('\n'), print_line(), print_board_row(ID, 1), print_board_row(ID, 2), print_board_row(ID, 3), print_board_row(ID, 4), print_board_row(ID, 5), print_floor(ID), print_line().
