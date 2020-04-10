:- consult(board).
:- consult(environment).

:- (dynamic player_score/2).

%-----------------Player ATTR----------------------------
set_score(ID, SCORE) :-
    SCORE=<0, !,
    (   retract(player_score(ID, _)), !
    ;   true
    ),
    assert(player_score(ID, 0)), !.
set_score(ID, SCORE) :-
    SCORE>0,
    (   retract(player_score(ID, _)), !
    ;   true
    ),
    assert(player_score(ID, SCORE)).


%Tells if from position (I,J) in the wall can be reached going through tiles the position (I,Goal)
can_reach_horiz(_, _, Goal, Goal) :- !.
can_reach_horiz(ID, I, J, Goal) :-
    J<Goal, !,
    Next is J+1,
    cell(ID, I, Next, _, 1),
    can_reach_horiz(ID, I, Next, Goal). 
can_reach_horiz(ID, I, J, Goal) :-
    J>Goal,
    Next is J-1,
    cell(ID, I, Next, _, 1),
    can_reach_horiz(ID, I, Next, Goal).

%Tells if from position (I,J) in the wall can be reached going through tiles the position (Goal,J)
can_reach_vert(_, Goal, _, Goal) :- !.
can_reach_vert(ID, I, J, Goal) :-
    I<Goal, !,
    Next is I+1,
    cell(ID, Next, J, _, 1),
    can_reach_vert(ID, Next, J, Goal). 
can_reach_vert(ID, I, J, Goal) :-
    I>Goal,
    Next is I-1,
    cell(ID, Next, J, _, 1),
    can_reach_vert(ID, Next, J, Goal).

%Calculate and leave in Amount the total adyacents in horizontal positions including position (I,J)
calculate_points_horizontal(ID, I, Color, Amount) :-
    cell(ID, I, J, Color, _),
    findall(1,
            ( member(Goal, [1, 2, 3, 4, 5]),
              can_reach_horiz(ID, I, J, Goal)
            ),
            Adyacents),
    length(Adyacents, Amount).

%Calculate and leave in Amount the total adyacents in vertical positions including position (I,J)
calculate_points_vertical(ID, I, Color, Amount) :-
    cell(ID, I, J, Color, _),
    findall(1,
            ( member(Goal, [1, 2, 3, 4, 5]),
              can_reach_vert(ID, I, J, Goal)
            ),
            Adyacents),
    length(Adyacents, Amount).


%-------------------------------Update Final Score---------------------------------------------------
%Triumph if C is the number of completed lines of player ID 
check_complete_rows(_, [], 0) :- !. 
check_complete_rows(ID, [R|T], C) :-
    check_line(ID, R, 5), !,
    check_complete_rows(ID, T, C2),
    C is C2+1. 
check_complete_rows(ID, [_|T], C) :-
    check_complete_rows(ID, T, C).

update_score_by_rows(ID, Cant) :-
    check_complete_rows(ID, [1, 2, 3, 4, 5], Cant),
    Points is 2*Cant,
    player_score(ID, S),
    NS is S+Points,
    set_score(ID, NS).  

check_column(ID, Col, B) :-
    findall(_,
            cell(ID, _, Col, _, 1),
            T),
    length(T, 5), !,
    B is 1.
check_column(_, _, 0).

check_complete_columns(_, 0, 0) :- !.
check_complete_columns(ID, Col, C) :-
    check_column(ID, Col, B),
    Col2 is Col-1,
    check_complete_columns(ID, Col2, C2),
    C is B+C2.

update_score_by_columns(ID) :-
    check_complete_columns(ID, 5, Cant),
    Points is 7*Cant,
    player_score(ID, S),
    NS is S+Points,
    set_score(ID, NS).  

check_color(ID, Color, B) :-
    findall(_,
            cell(ID, _, _, Color, 1),
            T),
    length(T, 5), !,
    B is 1.
check_color(_, _, 0).

check_all_colors(_, 0, 0) :- !.
check_all_colors(ID, Color, Cant) :-
    check_color(ID, Color, B),
    NColor is Color-1,
    check_all_colors(ID, NColor, Cant2),
    Cant is B+Cant2.

update_score_by_complete_colors(ID) :-
    check_all_colors(ID, 5, Cant),
    Points is 10*Cant,
    player_score(ID, S),
    NS is S+Points,
    set_score(ID, NS). 

update_final_score(ID, Score, Cant) :-
    update_score_by_rows(ID, Cant),
    update_score_by_columns(ID),
    update_score_by_complete_colors(ID),
    player_score(ID, Score).

update_all_scores(0, []) :- !.
update_all_scores(ID, [[Score, CompleteR, ID]|T]) :-
    update_final_score(ID, Score, CompleteR),
    ID2 is ID-1,
    update_all_scores(ID2, T).


max_score([[S, C, _]], S, C) :- !.
max_score([[S, C, _]|T], S, C) :-
    max_score(T, SS, _),
    S>SS, !.
max_score([[S, C, _]|T], S, CC) :-
    max_score(T, S, CC),
    CC>C, !.
max_score([[_, _, _]|T], SS, CC) :-
    max_score(T, SS, CC).

select_winners([], _, _, []).
select_winners([[X, Y, ID]|T], X, Y, [[X, Y, ID]|TT]) :- !,
    select_winners(T, X, Y, TT).
select_winners([[_, _, _]|T], X, Y, TT) :-
    select_winners(T, X, Y, TT).


print_win([S, _, ID]) :-
    printText('\n Player:', green),
    printText(ID, green),
    printText('  Score:', green),
    printText(S, green).

print_winners(_, 0) :- !.
print_winners([W|T], Cant) :-
    print_win(W),
    C is Cant-1,
    print_winners(T, C).

winners(C, LW, Size) :-
    update_all_scores(C, L),
    max_score(L, S, CR),
    select_winners(L, S, CR, LW),
    length(LW, Size).
