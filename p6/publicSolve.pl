/* find solutions for board x within n steps */

solve_board_test(B,N) :- findall(V, solve_board(B,N,V), R), write(R), nl.
solve_board_public :-
	solve_board_test([1,0,2,3],3),
	solve_board_test([1,3,2,0],3),
	solve_board_test([1,2,0,3,4,5,6,7,8],4),
	solve_board_test([3,1,2,6,4,5,7,8,0],4),
	solve_board_test([1,0,2,3,4,5,6,7,8],2),
	solve_board_test([1,0,2,3,4,5,6,7,8],12).
