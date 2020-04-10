:- consult(board).
:- consult(environment).
:- consult(strategy).
:- consult(player).

start_play(NumPlayers):- init_play(NumPlayers), create_players(NumPlayers).

%check if any player complete one line in the wall
check_end_play() :-  check_stop(1).

fase1(2) :- finish_fase1(), !.
fase1(1) :- next_turn(Next), print_table() , pick(Next), print_board(Next),  update_next_turn(), !.
fase1(5).

fase2(3) :- !, cant_players(Cant), update_all_walls(Cant). 
fase2(6).

fase3(4) :- check_end_play(), !, print_end(). 
fase3(1) :- prepare_next_round(), !.       % If prepare_next_round fail => Stop the play(bag empty)             
fase3(4) :- print_end_bag(). 

simulate(1) :- fase1(X), simulate(X).
simulate(2) :- printText('END FASE 1\n', red), fase2(X), simulate(X).
simulate(3) :- fase3(X), simulate(X).
simulate(4) :- cant_players(C), winners(C, W, S),  print_play_state(), printText('\n WINNERS !!!!!!', green),  print_winners(W, S).
simulate(5) :- printText('(Error in FASE 1)', red).
simulate(6) :- printText('(Error in FASE 2)', red).
