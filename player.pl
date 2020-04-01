:- consult(matrix).

:- dynamic(player_score/2, player_wall/2, player_stair/2, player_penalty/2).

%-----------------Player ATTR
set_score(ID, SCORE) :- ( (retract(player_score(ID, _)), !) ; true ) , assert(player_score(ID, SCORE)).
set_wall(ID, WALL):- ( (retract(player_wall(ID, _)), !) ; true ) , assert(player_wall(ID, WALL)).
set_stair(ID, STAIR):- ( (retract(player_stair(ID, _)), !) ; true ) , assert(player_stair(ID, STAIR)).
set_penalty(ID, PENALTY):- ( (retract(player_penalty(ID, _)), !) ; true ) , assert(player_penalty(ID, PENALTY)).

%-----------------Player Initialization
init_player(ID):- set_score(ID, 0), init_matrix_wall(ID), init_stair(ID), set_penalty(ID, 0). 

%-----------------Player Actions