/* list of positions that can move to space */

move_pos_test(B) :- findall(V, move_pos(B,V), R), write(R), nl.
move_pos_public :-
	move_pos_test([0,1,2,3]),
	move_pos_test([1,2,0,3]),
	move_pos_test([0,1,2,3,4,5,6,7,8]),
	move_pos_test([1,2,3,4,0,5,6,7,8]),
	move_pos_test([1,2,3,4,5,0,6,7,8]).

/* make move given (board, position of value to be moved) */

make_move_test(B,P) :- findall(V, make_move(B,P,V), R), write(R), nl.
make_move_public :-
	make_move_test([0,1,2,3],1),
	make_move_test([0,1,2,3],2),
	make_move_test([1,2,0,3],0),
	make_move_test([1,2,0,3],3),
	make_move_test([0,1,2,3,4,5,6,7,8],1),
	make_move_test([0,1,2,3,4,5,6,7,8],3),
	make_move_test([1,2,3,4,0,5,6,7,8],1),
	make_move_test([1,2,3,4,0,5,6,7,8],3),
	make_move_test([1,2,3,4,0,5,6,7,8],5),
	make_move_test([1,2,3,4,0,5,6,7,8],7),
	make_move_test([1,2,3,4,5,0,6,7,8],4),
	make_move_test([1,2,3,4,5,0,6,7,8],8).

/* make all possible moves for given board, return resulting boards */

make_moves_test(B) :- findall(V, make_moves(B,V), R), write(R), nl.
make_moves_public :-
	make_moves_test([0,1,2,3]),
	make_moves_test([1,2,0,3]),
	make_moves_test([0,1,2,3,4,5,6,7,8]),
	make_moves_test([1,2,3,4,0,5,6,7,8]),
	make_moves_test([1,2,3,4,5,0,6,7,8]).
