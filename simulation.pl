:- consult(matrix).
:- consult(environment).
:- consult(strategy).
:- consult(player).

start_play(NumPlayers):- init_play(NumPlayers), create_players(NumPlayers).

%check if any player complete one line in the wall
check_end_play() :- check_stop(1).

update_all_scores():- write("i am updating score success").


init_round():- first_player(F), set_next_turn(F), fase1(_).


fase1(2):- finish_fase1(), !.
fase1(1) :- next_turn(Next), print_play_state(),  pick(Next), print_board(Next), update_next_turn(), !.
fase1(6).

fase2(3):- !, cant_players(Cant), update_all_walls(Cant), !. 
fase2(5).

fase3(4) :- check_end_play(), ! , update_all_scores(). 
fase3(1) :- prepare_next_round(), !.                    %next round puede finalizar el juego
fase3(7) :- !.

simulate(1) :- fase1(X), simulate(X).
simulate(2) :- write('End Fase 1\n'), fase2(X), simulate(X).
simulate(3) :- fase3(X), simulate(X).
simulate(4) :- write("hacer lo que tengo que hacer cuando se acabe").
simulate(5) :- write("upss se rompio algo en la fase 2").
simulate(6) :- write("upss se rompio algo en la fase 1").
simulate(7) :- write("upss se rompio algo en la fase 3").