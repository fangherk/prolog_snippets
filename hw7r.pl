% insert_ord(+RL0, +Element, ?RL):
% RL0 is a proper list of numbers, which is strictly increasing. Element is a given number.
% The task is to return the strictly increasing list RL which is obtained from RL0
% by inserting Element to it, if Element is not already in RL0. Otherwise RL = RL0.

insert_ord([H|T], Elem, NL):-
	( Elem < H -> NL = [Elem,H|T]
	; Elem = H -> NL = [H|T]
	; insert_ord(T, Elem, NL0),
	  NL = [H|NL0]
	).

% length_list(+Len, ?List): List is a list whose length is Len.
length_list(Len, L) :-
	( Len = 0 -> L = []
	; L = [_|T], 
	  Len0 is Len - 1,
	  length_list(Len0, T)
	).
	
% nth1(+N, ?L, ?E): The N-th element of the (possibly open ended) list L
% is E. The head of a list is considered its 1st element. Argument N is
% guaranteed to be a positive integer.
nth1(N, [H|T], E) :-
	( N > 1 -> N0 is N - 1,
	  nth1(N0, T, E)
	  ; E = H
	).
	  
% split(+N, +List, -Front, -Back): Front is the list containing the first
% N elements of List, and Back is the list of the remaining elements of
% List. N can be assumed to be a given integer.
split(N, L, F, B) :-
	( N > 0 ->
	  L = [H|T],
	  N0 is N - 1,
	  split(N0, T, F0, B),
	  F = [H|F0]
	; N = 0 -> F = [], B = L 
	).

% chop(+N, +List, -LofLists): LofLists is a list whose elements are
% nonempty lists, such that the concatenation of these results in List.
% All elements of LofLists, except for the last, have length N, the
% length of the last should be between 1 and N (inclusive).
chop(N, L, LL):-
	length(L, M),
	( N =< M -> 
	  split(N, L, F, B),
	  chop(N, B, LL0),
	  LL = [F|LL0]
	; L \= [] -> LL = [L]
	; L = [] -> LL = []
	).

% visible_count(+L, ?N): N is the number of left-visible elements in
% the proper list L of positive integers
visible_count([], _, 1).
visible_count([H|T], Max, N) :-
	( H > Max ->
	  visible_count(T, H, N0),
	  N is N0 + 1
	; visible_count(T, Max, N)
	).

visible_count([H|T], N):-
	visible_count(T, H, N).