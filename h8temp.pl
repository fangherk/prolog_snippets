:- use_module(library(lists), [select/3]).
% sublist(+Whole, ?Part, ?Before, ?Length, ?After): Part is a
% sublist of Whole such that there are Before number of elements in
% Whole before Part, After number of elements in Whole after Part
% and the length of Part is Length
sublist(Whole, Part, Before, Length, After):-
	append(TempB, TempC, Whole),
	length(TempB, Before),
	append(Part, TempA, TempC),
	length(Part, Length),
	length(TempA, After).

% coeff(Expr, A): The coefficient of 'x' in the linear expression Expr is A
coeff(x, 1).
coeff(C, 0) :- number(C).
coeff(A+B, CC) :- coeff(A, AA), coeff(B,BB), CC is AA + BB.
coeff(A-B, CC) :- coeff(A, AA), coeff(B,BB), CC is AA - BB.
coeff(A*B, CC) :-
	(number(B) -> 
	  coeff(A, TT),
	  D is B
	; number(A) ->
	  coeff(B, TT),
	  D is A
	),
	CC is TT*D.

% plateau(L, I, Len): There is a plateau of length Len starting at the
% I-th position of list L.
%helperCount(X, [X], 1).
%helperCount(X, [Y|_], 0) :- X \= Y.
%helperCount(X, [X|L], Len) :-
%	helperCount(X, L, Len0),
%	Len is Len0+1.
%endElement([], []).
%endElement([X], X).
%endElement([_|L], M) :-
%	L \= [],
%	endElement(L, M).
%
%plateau(L, I, Len) :-
%	append(Before, [X|R], L),
%	endElement(Before, P),
%	P \= X,
%	helperCount(X, [X|R], Len),
%	Len > 1,
%	length(Before, I0),
%	I is I0+1.


plateau([X|L], I, Len):-
	plateauHelper(L, [X], I, Len).


plateauHelper([], _, 0, 0).
plateauHelper([X], [], 1, 2).
plateauHelper([X|L], [E|Y], I, Len) :-
	( X = E ->
	  plateauHelper(L, [X|Y], I, Len0)
	; plateauHelper(L, X, I0, Len),
	  I is I0+1
	),
	length([E|Y], Len).

%plateauHelper([X|L], [], 0, 0 , Len) :-
%	X \= Y, length([Y|R], Len).
%plateauHelper([X,Y|L], Accum, Count, I, Len) :-
%	( X \= Y ->  
%	  length(Accum, Len),
%	  Len > 2.
%	; plateauHelper(L, [X,Y|Accum], 
%plateau([X|L], I, Len):-
%	plateau(L, X, I, Len).
%plateau([X,X], 1, 2).
%plateau([X,Y|L], I, Len):-
%	( X \= Y ->
%	  plateau([Y|L], I, Len)
%	; X =:= Y ->
%	  plateau([Y|L], X, I0, Len0),
%	  I is I0+1,
%	  Len is Len+1.
%	  ).

optional_plateau.
% draw(+G, -L): Graph G can be drawn by line L
% draw(+G, ?P, -L): Graph G can be drawn by line L which starts at point P
% (in other words: L = [P-_|_]

draw(List, L) :-
	draw(List, _, L).
draw([], _, []).
draw(Graph, P, [P-Q|L]):-
	(
	 select(P-Q, Graph, Rest)
	;
	 select(Q-P, Graph, Rest)
	),
	draw(Rest, Q, L).
	
end_of_file.
