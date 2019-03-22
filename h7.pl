% RL0 is a proper list of numbers, which is strictly increasing. Element is a given number.
% The task is to return the strictly increasing list RL which is obtained from RL0
% by inserting Element to it, if Element is not already in RL0. Otherwise RL = RL0.
insert_ord([], E, [E]).
insert_ord([X|RL0], E, RL) :-
	(   E < X ->   RL = [E, X|RL0]
	;   E > X, RL0 = [] ->   RL = [X, E]
	;   E = X ->   insert_ord(RL0, E, RL)
	;   insert_ord(RL0, E, R2),
	    RL = [X|R2]
	).

% length_list(+Len, ?List): List is a list whose length is Len.
length_list(0, []).
length_list(Len0, [_|T]) :-
	Len0 > 0,
	Len1 is Len0-1,
	length_list(Len1, T).
	
%nth1(+N, ?L, ?E): The N-th element of the (possibly open ended) list L
% is E. The head of a list is considered its 1st element. Argument N is
% guaranteed to be a positive integer.
nth1(1, [H|_], H).
nth1(N, [_|R], E) :-
	N > 0,
	N1 is N-1,
	nth1(N1, R, E).
	
% split(+N, +List, -Front, -Back): Front is the list containing the first
% N elements of List, and Back is the list of the remaining elements of
% List. N can be assumed to be a given integer.
split(0, List, [], List).
split(N, [H|T], Front, Back) :-
	N1 is N-1,
	split(N1, T, TFront, Back),
	Front = [H|TFront].
	
% chop(+N, +List, -LofLists): LofLists is a list whose elements are
% nonempty lists, such that the concatenation of these results in List.
% All elements of LofLists, except for the last, have length N, the
% length of the last should be between 1 and N (inclusive).
chop(N, List, LL):-
	(   length(List, T), T >= N -> split(N, List, Front, Back),
	    chop(N, Back, LL0),
	    LL = [Front|LL0]
	;   List = [] -> LL = List
	;   LL = [List]
	).
	
% visible_count(+L, ?N): N is the number of left-visible elements in
% the proper list L of positive integers
visible_count(L, N) :-
	visible_helper(0, L, N).
	
% visible_helper(+X, +L, ?N): N is the the number of left visible elments
% L is the list that is compared to the most recent max X
% N is the count of elments which are incremented each time the
% head of L is larger than the current max X 
visible_helper(_, [], 0).
visible_helper(X, [H|T], N) :-
	(   H > X -> visible_helper(H, T, N0),
	    N is N0 + 1
	;   visible_helper(X, T, N)
	).
	 
end_of_file.
