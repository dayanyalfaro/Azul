
%----------------------Utils----------------------------------

% help method to erase from a list L index i, V = L[i]
remove([X|L], 1, L, X) :- !.
remove([X|L], K, [X|R], V) :- K1 is K - 1, remove(L, K1, R, V).

remove_first([], [], -1).
remove_first([X|R], R, X).

concat([], L, L).
concat([X|L1], L2, [X|R]):- concat(L1, L2, R).

printB(SMS):- !, ansi_format([bold, fg(blue)], SMS, []).
printY(SMS):- !, ansi_format([bold, fg(yellow)], SMS, []).
printR(SMS):- !, ansi_format([bold, fg(red)], SMS, []).
printG(SMS):- !, ansi_format([bold, fg(green)], SMS, []).
printW(SMS):- !, ansi_format([bold, fg(white)], SMS, []).

printcolor(1):- printB('1'), !.
printcolor(2):- printY('2'), !.
printcolor(3):- printR('3'), !.
printcolor(4):- printG('4'), !.
printcolor(5):- printW('5'), !.