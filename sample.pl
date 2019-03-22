sum([], 0).
sum([X|L], R) :-
	sum(L, R1),
	R is X + R1.

%pow(_, 0, 1).
%pow(A, E, P) :-
%	E0 is E - 1,
%	pow(A, E0, P0),
%	P is A0 * P0.

pow(A, E, P) :-
	( E > 0 -> E1 is E-1,
	  pow(A, E1, P1),
	  P is A*P1
	; E = 0 -> P = 1 ).
	
	
end_of_file.