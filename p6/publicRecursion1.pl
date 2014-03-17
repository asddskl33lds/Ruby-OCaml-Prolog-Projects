/* return n'th element of list, or fail if not found */

get_val_test(L,N) :- findall(V, get_val(L,N,V), R), write(R), nl.
get_val_public :-
	get_val_test([5,6,7,3],0),
	get_val_test([5,6,7,3],1),
	get_val_test([5,6,7,3],2),
	get_val_test([5,6,7,3],3),
	get_val_test([5,6,7,3],4).

/* list of values at list of indices */
	
get_vals_test(L,N) :- findall(V, get_vals(L,N,V), R), write(R), nl.
get_vals_public :-
	get_vals_test([5,6,7,3],[0]),
	get_vals_test([5,6,7,3],[1]),
	get_vals_test([5,6,7,3],[3]),
	get_vals_test([5,6,7,3],[0,2]),
	get_vals_test([5,6,7,3],[0,2,1]),
	get_vals_test([5,6,7,3],[0,2,1,3]),
	get_vals_test([5,6,7,3],[4]).

/* set n'th element of list x to value v, return resulting list */
	
set_n_test(L,N,X) :- findall(V, set_n(L,N,X,V), R), write(R), nl.
set_n_public :-
	set_n_test([5,6,7,3],0,9),
	set_n_test([5,6,7,3],1,9),
	set_n_test([5,6,7,3],2,9),
	set_n_test([5,6,7,3],3,9),
	set_n_test([5,6,7,3],4,9).
