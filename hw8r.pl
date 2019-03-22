% sublist(+Whole, ?Part, ?Before, ?Length, ?After): Part is a
% sublist of Whole such that there are Before number of elements in
% Whole before Part, After number of elements in Whole after Part
% and the length of Part is Length.

sublist(Whole, Part, Before, Length, After) :-
	append(PartA, PartB, Whole),
	length(PartA, Before),
	append(Part, PartY, PartB),
	length(Part, Length),
	length(PartY, After).

% coeff(Expr, A): The coefficient of ’x’ in the linear expression Expr is 
coeff(x, 1).
coeff(C, 0) :- number(C).
coeff(A-B, R):-
	coeff(A, I1),
	coeff(B, I2),
	R is I1 - I2.
coeff(A+B, R):-
	coeff(A, I1),
	coeff(B, I2),
	R is I1 + I2.
coeff(A*B, R):-
	( number(B) -> coeff(A, I1),
	  R is B*I1
	; number(A) -> coeff(B, I2),
	  R is A*I2
	).

% plateau(L, I, Len): There is a plateau of length Len starting at the
% I-th position of list L

plateau_helper([], _, 1, 0).
plateau_helper([E|R], Prev, I, Len) :-
	( E = Prev -> plateau_helper(R, E, I0, Len0),
	  Len is Len0 +1,
	  I = I0
	; Len >= 2 -> I = I0
        ;
	  R \= [] -> plateau_helper(R, E, I0, Len),
	  I is I0 + 1
	).
	


plateau(L, I, Len) :-
	plateau_helper(L, _, I, Len).
