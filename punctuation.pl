:- consult(matrix).

:- (dynamic player_score/2).

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