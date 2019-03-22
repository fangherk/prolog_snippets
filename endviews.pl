% Prolog Final Test/CLP part
% The End View Puzzle
% Herrick Fang

:- use_module(library(clpfd)).
:- use_module(library(lists)).

% countDown(+M, +List):
% The number of elements from 1 to M appears exactly once.
countDown(0, _) :- !.
countDown(M, List) :-
	M #>= 1,
	count(M, List, #=, 1),
	M0 is M-1,
	countDown(M0, List).

% firstpos(+K, +List, ?B): If the boolean (0-1 valued) variable B = 1, then the first positive number in list L is K.
firstPos(_, [], _).
firstPos(K, [H|T], B):-
	 B #= 1 #/\ H #= 0 #<=> B1,
	 B #= 1 #=> H in {0, K},
	 firstPos(K, T, B1).


% firstPos(+K, +List) : The first positive element of list is K. 
firstPos(K, [H|T]) :-
	H in {0, K},
	H #= 0 #<=> B,
	firstPos(K, T, B).

% endviews(+N, +M, ?List, +LV, +RV):
% The list List is of length N, and contains all numbers between 1 and M exactly once, and
% (N-M) instances of the number 0. LV (left view) is the first nonzero element in the list
% List and RV (right view) is the last nonzero element. All arguments, except List, are given
% integers, where N>=M, 1 =< LV =< M and 1 =< RV =< M also holds. Furthermore, LV \= RV
% holds unless M = 1. List is a variable or a proper list of FD variables/integers.
endviews(N, M, List, LV, RV) :-
	length(List, N),
	domain(List, 0, M),
	NumZeros is N-M,
	count(0, List, #=, NumZeros),
	countDown(M, List),
	firstPos(LV, List),
	reverse(List, TList),
	firstPos(RV, TList).

end_of_file.
	