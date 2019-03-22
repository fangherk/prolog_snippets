gt(X) :-
	X > 0.

filter0(Pred, L, FL) :-
	findall(X, (member(X, L), call(Pred,X)), FL).


filter1([], _, []).
filter1([X|L0], Pred, L):-
	(call(Pred,X) ->
	 L = [X|L1],
	 filter1(L0, Pred, L1)
	;
	 filter1(L0, Pred, L)
	 ).
	
square(X,Y) :- Y is X*X.
maplist0(_, [], []).
maplist0(PG, [X|L], [Y|ML]):-
	call(PG, X, Y),
	maplist0(PG, L, ML). 
end_of_file.
