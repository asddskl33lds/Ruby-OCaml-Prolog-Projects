/* swap values u,v in list b, assume all list elements are unique */

list_swap_test(L,X,Y) :- findall(V, list_swap_val(L,X,Y,V), R), write(R), nl.
list_swap_public :-
	list_swap_test([5,6,7,3],7,5),
	list_swap_test([5,6,7,3],6,5),
	list_swap_test([5,6,7,3],3,5),
	list_swap_test([5,6,7,3],7,3).

/* returns index of value v in list x, if found */

index_test(L,X) :- findall(V, index(L,X,V), R), write(R), nl.
index_public :-
	index_test([5,6,7,3],5),
	index_test([5,6,7,3],6),
	index_test([5,6,7,3],7),
	index_test([5,6,7,3],3),
	index_test([7,5,6,5],5),
	index_test([7,5,6,5],4).
