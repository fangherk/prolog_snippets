
% app(L1, L2, L3):            The concatenation of L1 and L2 is L3.
append([], L, L).              % The concatenation of [] and L is L.
append([X|L1], L2, [X|L3]) :-  % The concatenation of [X|L1] and L2 is [X|L3] if
	append(L1, L2, L3).     %           the concatenation of L1 and L2 is L3.

% nrev(L, R): List R is the reverse of list L.
nrev([], []).
nrev([H|T], R) :-
	append(R1, [H], R),
	nrev(T, R1).
	
% revapp(L1, L2, R): The reverse of L1 prepended to L2 gives L3
revapp([], L2, L2).
revapp([H|T], L2, L3) :-
	revapp(T, [H|L2], L3).
	
% reverse(L, R): List R is an efficient reverse of list L.
reverse(L, R) :-
	revapp(L, [], R).

% in_pair(+List, ?E, ?I):  E is an element of List equal to its
% right neighbour, occurring at position I (numbered from 1).
%in_pair([E,E|R], E0, I) :-
%	E0 = E,
%	I is 1
%	;
%	in_pair(R, E0, I0),
%	I is I0 + 2.
%in_pair([A,B|R], E, I) :-
%	A \= B,
%	in_pair([B|R], E, I0),
%	I is I0+1.
in_pair([E,E|_], E, 1).
in_pair([_|T], E, I) :-
	in_pair(T, E, I0),
	I is I0+1.

% append(L1, L2, L3, L123): L123 is the concatenation of L1, L2, and L3
append(L1, L2, L3, L123) :-
	append(L1, L23, L123),
	append(L2, L3, L23).

% append(LL, L):  LL is a proper list whose elements are (possibly open ended) lists.
% L is a concatenation of all the lists in LL.
append([], []).
append([H|T], L) :-
	append(H, L0, L),
	append(T, L0).

% select(Elem, List, Rest): Elem is an element of List, and removing it from
% List results in list Rest.
select(Elem, [Elem|L], L).
select(Elem, [Diff|L], [Diff|R0]) :-
	select(Elem, L, R0).



	
	