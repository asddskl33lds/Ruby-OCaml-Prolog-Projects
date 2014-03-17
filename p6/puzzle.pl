/* NAME -SARTHI ANDLEY
HELPER FUNCTIONS */

/* len - find length of list */

len([],0).
len([_|T],Result) :-
	len(T,R),
	Result is R+1.
	

my_rev(X,F) :- rev_helper(X,[],F).

rev_helper([],A,A).
rev_helper([H|T],A,F) :- 
	rev_helper(T,[H|A],F).

	
/* is_sorted - whether elements of list are in sorted order */

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|Z]) :- 
	(X<Y),
	is_sorted([Y|Z]).

/* find_board_size - find height/width of board in list form */

find_board_size(B,Result) :- 
	len(B,L),
	fHelper(L,Result).
fHelper(4,2).
fHelper(9,3).
fHelper(16,4).
fHelper(25,5).
fHelper(36,6).
fHelper(49,7).
fHelper(64,8).

/* pos_of_xy */

pos_of_xy(X,Y,S,Result) :-
	X > -1,
	X =< S-1,
	Y > -1,
	Y =< S-1,
	T1 is X*S,
	Result is T1+Y.
	
/* xy_of_pos */

xy_of_pos(P,S,X,Y) :-
	X is P div S,
	Y is P mod S.

/*--------------------------------------------------------------*/
/* PROJECT FUNCTIONS */

/* get_val - return N'th element of list L, or fail if not found */


get_val([H|_],0,H).

get_val([_|T],N,R) :- 
			Poop is N-1,
			get_val(T,Poop,R). 

/* get_vals - return list of values in L at list of indices N */

set([],Acc, Acc). 
	
get_vals(X,Y,R) :- get_vals_helper(X,Y,[],R1),
					my_rev(R1,R).

get_vals_helper(_,[], Accum, Accum).

get_vals_helper(X,[H|T],Accum, R) :-
     get_val(X,H,R1),
	 get_vals_helper(X,T,[R1|Accum], R).


	
/* set_n - set N'th element of list N to value V, return result */



set_n([_|Y], 0, V, [V|Y]).

set_n(X, N, _, R) :- 
							len(X,L),
							N>L-1,
							set([],X,R).

set_n([H|T], N, V, [H|R]) :- 
							N > 0,
							len([H|T],L),
							N=<L-1,
							N1 is N-1,
							set_n(T, N1, V, R).
										

/* list_swap_val - swap values U, V in list B */



list_swap_val(B,U,V,Result) :- swap_helper(B,U,V,[],R),
my_rev(R,Result).

swap_helper([],_ , _ , Accum, Accum).

swap_helper([H|T],U,V,Accum,R) :- 
				H\==U,
				H\==V,
				swap_helper(T,U,V,[H|Accum],R).
				
swap_helper([H|T],H,V, Accum,R) :- 
				swap_helper(T,H,V,[V|Accum], R).

swap_helper([H|T],U,H, Accum,R) :- 
				swap_helper(T,U,H,[U|Accum], R).

/* returns index of value V in list X, if found */

index([V|_],V,0).

index([_|T],V,R) :- 
				index(T,V,Ans),
				R is Ans+1.

/* list of positions in board B that can move to space  */

move_pos(B,R) :- 
				index(B,0, Izero),
				find_board_size(B,S),
				xy_of_pos(Izero,S,X,Y),
				pos_of_xy(X-1,Y,S,R).

move_pos(B,R) :- 
				index(B,0, Izero),
				find_board_size(B,S),
				xy_of_pos(Izero,S,X,Y),
				pos_of_xy(X,Y-1,S,R).
			
move_pos(B,R) :- 
				index(B,0, Izero),
				find_board_size(B,S),
				xy_of_pos(Izero,S,X,Y),
				pos_of_xy(X,Y+1,S,R).
				
move_pos(B,R) :- 
				index(B,0, Izero),
				find_board_size(B,S),
				xy_of_pos(Izero,S,X,Y),
				pos_of_xy(X+1,Y,S,R).

/* make move given board B and position X of value to be moved */

make_move(B,X,R) :- 
	get_val(B,X,E),
    list_swap_val(B,0,E,R).

/* make all possible moves for given board B, return resulting boards */

make_moves(B,R) :-
	move_pos(B,R1),
	make_move(B,R1,R).


/* find solutions for board B within N steps */

solve_help(_,0,_) :- fail.
				
solve_help([H|T],N,R) :- 
				N>0,
				is_sorted(H),
				set([],[H|T],R).

solve_help([H|T],N,R) :- 
				N>0,
				set([],[H|T],X),
				\+(is_sorted(H)),
				make_moves(H,R2),
				\+(index([H|T],R2,_)),
				set([],[R2|X],L),
				solve_help(L,N-1,R).

solve_board(B,N,R) :- solve_help([B],N+1,R).

