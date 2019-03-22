p([A,B|_], D, R) :- A < B-D, R = B.
p([A|As], D, R) :- p(As, D, R).

p(L,Z) :- p(L, 0, Z).


first_aps([X], S) :- X > 0, S = [X].
first_aps([A,B|R], S) :-
	( A > 0, B >= A ->
	  first_aps([B|R], S0),
	  append([A], S0, S)
	; A > 0 -> S = [A]
	; first_aps([B|R], S)
	).

pp([_|Xs], A, Y) :-
	A1 is A+1,
	pp(Xs, A1, Y).
pp([X|Xs], X, X).

pos_avg([H|T], A) :- pos_avg([H|T], 0, 0, A).
pos_avg([], S, C, A) :- C > 0, A is S/C.
pos_avg([X|L], S, C, A) :-
	( X > 0 -> S1 is S + X,
	  C1 is C + 1,
	  pos_avg(L, S1, C1, A)
	; pos_avg(L, S, C, A)
	).

well_hidden([F|R], X) :-
	well_hidden(R, F, X).

well_hidden([F|R], M, E):-
	( F < M ->
	  ( E = F
	  ; well_hidden(R, F, E)
	  )
	; well_hidden(R, M, E)
	).
	
ppp([X|L], Z) :- ppp(L, X, Z).
ppp([Y,Z|_], X, Z) :- Z =:= X+Y.
ppp([X|L], _, Z) :- ppp(L, X, Z).

end_of_file.

1. a. L = [], A = a. % -->  Resolve first variable first
   b. X = a, Y = [b]. % good
   c. U = 5+7, V = 2.  %  good
   d. Instantiation error. % 
   e. A = 8, B = 8+1. % 
   f. Z = 1+5. % Wrong. Remember that assignment doesn't get resolved.

2. a. X = c, U = a/c +b, Y = a/c, Z = b. %% done
   b. W = 2, V = 3, L = [3], I = a, J = b %% done
   
