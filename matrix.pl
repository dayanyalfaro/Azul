
:- dynamic(cell/5, stair/4).



%-----------------Matrix methods
%Color Definition
% 1 - Blue
% 2- Yellow
% 3 - Red 
% 4 - Black
% 5 - White

init_wall(
    [
        [1,2,3,4,5],
        [5,1,2,3,4],
        [4,5,1,2,3],
        [3,4,5,1,2],
        [2,3,4,5,1]
    ]).

init_matrix_wall_col(ID, I, [A,B,C,D,E]) :- 
    assert(cell(ID, I, 1, A, 0)), assert(cell(ID, I, 2, B, 0)), assert(cell(ID, I, 3, C, 0)), assert(cell(ID, I, 4, D, 0)), assert(cell(ID, I, 5, E, 0)).

init_matrix_wall(ID) :- init_wall([ A, B, C, D, E ]), 
    init_matrix_wall_col(ID, 1, A), init_matrix_wall_col(ID, 2, B), init_matrix_wall_col(ID, 3, C), init_matrix_wall_col(ID, 4, D), init_matrix_wall_col(ID, 5, E). 

set_value_wall(ID, I, J, V) :- retract(cell(ID, I, J, C, _)), assert(cell(ID, I, J, C, V)).

init_stair(ID):- assert(stair(ID, 1, 1, 0)), assert(stair(ID, 2, 1, 0)), assert(stair(ID, 2, 2, 0)), assert(stair(ID, 3, 1, 0)), assert(stair(ID, 3, 2, 0)), assert(stair(ID, 3, 3, 0)), assert(stair(ID, 4, 1, 0)), assert(stair(ID, 4, 2, 0)), assert(stair(ID, 4, 3, 0)), assert(stair(ID, 4, 4, 0)), assert(stair(ID, 5, 1, 0)), assert(stair(ID, 5, 2, 0)), assert(stair(ID, 5, 3, 0)), assert(stair(ID, 5, 4, 0)), assert(stair(ID, 5, 5, 0)).
set_value_stair(ID, I, J, Color):- retract(stair(ID, I, J, _)), assert(stair(ID, I, J, Color)).

selectk([X|_], 1, X):- !.
selectk([_|R], K, X):- K1 is K - 1, selectk(R, K1, X).

 %---------------------Print Board----------------------------
print_cell(1 , 0):- ansi_format([bold, fg(blue)], '(_) ', []).
print_cell(1 , 1):- ansi_format([bold, fg(blue)], '(X) ', []).
print_cell(2 , 0):- ansi_format([bold, fg(yellow)], '(_) ', []).
print_cell(2 , 1):- ansi_format([bold, fg(yellow)], '(X) ', []).
print_cell(3 , 0):- ansi_format([bold, fg(red)], '(_) ', []).
print_cell(3 , 1):- ansi_format([bold, fg(red)], '(X) ', []).
print_cell(4 , 0):- ansi_format([bold, fg(black)], '(_) ', []).
print_cell(4 , 1):- ansi_format([bold, fg(black)], '(X) ', []).
print_cell(5 , 0):- ansi_format([bold, fg(white)], '(_) ', []).
print_cell(5 , 1):- ansi_format([bold, fg(white)], '(X) ', []).


print_row([]):- write('\n').
print_row([X|T]):- selectk(X, 1, C), selectk(X, 2, S), print_cell(C, S), print_row(T).
 
select_row(ID, 1, _, T):- findall([Color, State], cell(ID, 1, _, Color, State), T ), print_row(T).
select_row(ID, 2, _, T):- findall([Color, State], cell(ID, 2, _, Color, State), T ), print_row(T).
select_row(ID, 3, _, T):- findall([Color, State], cell(ID, 3, _, Color, State), T ), print_row(T).
select_row(ID, 4, _, T):- findall([Color, State], cell(ID, 4, _, Color, State), T ), print_row(T).
select_row(ID, 5, _, T):- findall([Color, State], cell(ID, 5, _, Color, State), T ), print_row(T).

print_board(ID, 5):- select_row(ID, 5, _, _) , !.
print_board(ID, R) :- select_row(ID, R, _, _), R1 is R + 1, print_board(ID, R1).
