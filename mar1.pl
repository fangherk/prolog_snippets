pow(A, E, P):-
	( E > 0 -> E1 is E-1,
	  pow(A, E1, P1),
	  P is A*P1;
	  E=0 -> P =1
	).


has_sibling(A,B).
not_has_sibling :-
	( has_sibling(A, B) -> false
	; true
	).


betwee(N, M, N).
betwee(N, M, X) :-
	betwee(N, M, N).
		