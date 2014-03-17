my_rev(X,R) :- rev_helper(X,[],R).

rev_helper([],R,R).

rev_helper([H|T],A,R) :- rev_helper(T,[H|A], R).
	
set_n([],_,_,[]) :- fail.


my_concat([], L2, L2).
my_concat([Elmt | L1], L2, [Elmt | C]) :- 
	my_concat(L1, L2, C).

set(R,R).

set_n(X,N,V,R) :- set_n_helper(X,N,V,[],R2),
					my_rev(R2,R).
	
set_n_helper([_|T], 0, V, A, R2) :- set(R3,[V|T]),
									my_rev(R3,R4),
									my_concat(R4,A,R2).

set_n_helper([H|T],N,V,R1,R2) :-
	N>0,
	N1 is N-1,
	set_n_helper(T,N1,V,[H|R1], R2).