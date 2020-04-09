:- dynamic(cell/5, stair/4).

%----------------------Utils----------------------------------
remove([X|L], 1, L, X) :- !.
remove([X|L], K, [X|R], V) :-
    K1 is K-1,
    remove(L, K1, R, V).

remove_first([], [], -1).
remove_first([X|R], R, X).

concat([], L, L).
concat([X|L1], L2, [X|R]) :-
    concat(L1, L2, R).



%--------------------Print Methods---------------------------------------------------------------

printText(S, Color):- ansi_format([bold, fg(Color)], S, []).

print_end():-  printText('\n************************************************* END GAME ***********************************************\n', red).
print_end_bag():- printText('\n************************************************* END GAME (Bag is empty) ***********************************************\n', red).
print_line():-  write('-----------------------------------------------\n').

printcolor(1) :- printText('1', blue), !.
printcolor(2) :- printText('2', yellow), !.
printcolor(3) :- printText('3', red), !.
printcolor(4) :- printText('4', green), !.
printcolor(5) :- printText('5', white), !.

source(0):- printText('Center', white), !.
source(X):- printText('Factory ', white), printText(X, white), !.

destiny(0):- printText('floor', white), !.
destiny(Row):- printText('row ', white), printText(Row, white), !.

printPick([Source,Color,Amount,Stair,_]) :- printText('  Takes ', white), printText(Amount, white), printText(' tiles of color ', white), printcolor(Color), printText(' from ', white), source(Source), printText(' and put them in the ', white), destiny(Stair), write('\n'), !.



 % Print the wall cells
 print_cell(0, 1) :-  ansi_format([bold, fg(black)], '(_) ', []), !.  %is empty the floor cell
 print_cell(-1, 1):- ansi_format([bold, fg(white)], '(1) ', []), !. %is chip 1 in the floor cell
 print_cell(1, 0) :- ansi_format([bold, fg(blue)], '(_) ', []), !.
 print_cell(1, 1) :- ansi_format([bold, fg(blue)], '(X) ', []), !.
 print_cell(2, 0) :- ansi_format([bold, fg(yellow)], '(_) ', []), !.
 print_cell(2, 1) :- ansi_format([bold, fg(yellow)], '(X) ', []), !.
 print_cell(3, 0) :- ansi_format([bold, fg(red)], '(_) ', []), !.
 print_cell(3, 1) :- ansi_format([bold, fg(red)], '(X) ', []), !.
 print_cell(4, 0) :- ansi_format([bold, fg(green)], '(_) ', []), !.
 print_cell(4, 1) :- ansi_format([bold, fg(green)], '(X) ', []), !.
 print_cell(5, 0) :- ansi_format([bold, fg(white)], '(_) ', []), !.
 print_cell(5, 1) :- ansi_format([bold, fg(white)], '(X) ', []), !.
 
 % Print the stair cells
 print_stair(0) :- ansi_format([bold, fg(cyan)], '(_) ', []), !.
 print_stair(1) :- ansi_format([bold, fg(blue)], '(X) ', []), !.
 print_stair(2) :- ansi_format([bold, fg(yellow)], '(X) ', []), !.
 print_stair(3) :- ansi_format([bold, fg(red)], '(X) ', []), !.
 print_stair(4) :- ansi_format([bold, fg(green)], '(X) ', []), !.
 print_stair(5) :- ansi_format([bold, fg(white)], '(X) ', []), !.
 
 % Print the stair rows
 print_stair_row(ID, 1) :- write('|                '), stair(ID, 1, 1, S), print_stair(S), write('  |  '), !.
 print_stair_row(ID, 2) :- write('|            '), stair(ID, 2, 2, S1), print_stair(S1), stair(ID, 2, 1, S2), print_stair(S2), write('  |  '), !.
 print_stair_row(ID, 3) :-
     write('|        '),
     stair(ID, 3, 3, S1),
     print_stair(S1),
     stair(ID, 3, 2, S2),
     print_stair(S2),
     stair(ID, 3, 1, S3),
     print_stair(S3),
     write('  |  '), !.
 print_stair_row(ID, 4) :-
     write('|    '),
     stair(ID, 4, 4, S1),
     print_stair(S1),
     stair(ID, 4, 3, S2),
     print_stair(S2),
     stair(ID, 4, 2, S3),
     print_stair(S3),
     stair(ID, 4, 1, S4),
     print_stair(S4),
     write('  |  '), !.
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
     write('  |  '), !.
 
 % Print the wall rows
 print_wall_row(ID, 1) :-
     cell(ID, 1, 1, C1, S1),
     print_cell(C1, S1),
     cell(ID, 1, 2, C2, S2),
     print_cell(C2, S2),
     cell(ID, 1, 3, C3, S3),
     print_cell(C3, S3),
     cell(ID, 1, 4, C4, S4),
     print_cell(C4, S4),
     cell(ID, 1, 5, C5, S5),
     print_cell(C5, S5), !.
 print_wall_row(ID, 2) :- 
     cell(ID, 2, 1, C1, S1),
     print_cell(C1, S1),
     cell(ID, 2, 2, C2, S2),
     print_cell(C2, S2),
     cell(ID, 2, 3, C3, S3),
     print_cell(C3, S3),
     cell(ID, 2, 4, C4, S4),
     print_cell(C4, S4),
     cell(ID, 2, 5, C5, S5),
     print_cell(C5, S5), !.
 print_wall_row(ID, 3) :- 
     cell(ID, 3, 1, C1, S1),
     print_cell(C1, S1),
     cell(ID, 3, 2, C2, S2),
     print_cell(C2, S2),
     cell(ID, 3, 3, C3, S3),
     print_cell(C3, S3),
     cell(ID, 3, 4, C4, S4),
     print_cell(C4, S4),
     cell(ID, 3, 5, C5, S5),
     print_cell(C5, S5), !.
 print_wall_row(ID, 4) :-
     cell(ID, 4, 1, C1, S1),
     print_cell(C1, S1),
     cell(ID, 4, 2, C2, S2),
     print_cell(C2, S2),
     cell(ID, 4, 3, C3, S3),
     print_cell(C3, S3),
     cell(ID, 4, 4, C4, S4),
     print_cell(C4, S4),
     cell(ID, 4, 5, C5, S5),
     print_cell(C5, S5), !.
 print_wall_row(ID, 5) :-
     cell(ID, 5, 1, C1, S1),
     print_cell(C1, S1),
     cell(ID, 5, 2, C2, S2),
     print_cell(C2, S2),
     cell(ID, 5, 3, C3, S3),
     print_cell(C3, S3),
     cell(ID, 5, 4, C4, S4),
     print_cell(C4, S4),
     cell(ID, 5, 5, C5, S5),
     print_cell(C5, S5), !.
 
 %Print the board rows
 print_board_row(ID, 1) :- 
     print_stair_row(ID, 1) , print_wall_row(ID, 1), write('| \n'), !.
 print_board_row(ID, 2) :- 
     print_stair_row(ID, 2),
     print_wall_row(ID, 2),
     write('| \n'), !.
 print_board_row(ID, 3) :- 
     print_stair_row(ID, 3),
     print_wall_row(ID, 3),
     write('| \n'), !.
 print_board_row(ID, 4) :- 
     print_stair_row(ID, 4),
     print_wall_row(ID, 4),
     write('| \n'), !.
 print_board_row(ID, 5):-  print_stair_row(ID, 5), print_wall_row(ID, 5), write('| \n'), !.