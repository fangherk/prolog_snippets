%1. nrev(L, R): List R is the reverse of list L.
%| ?- nrev([], R).                          R = [] ? ; no
%| ?- nrev([a,b,c], R).                     R = [c,b,a] ? ; no

nrev([], R) :- R = [].
nrev([X1|L1], R) :-
	nrev(L1, R1),
	append(R1, [X1], R).
% n^2 / 2 steps neededj

%2. revapp(L1, L2, R): The reverse of L1 prepended to L2 gives L3.
%| ?- revapp([a,b,c], [d,e], L).            L = [c,b,a,d,e] ? ; no
%| ?- revapp([], [d,e], L).                 L = [d,e] ? ;
% Based on revapp/3 define reverse/2 which is an efficient version of nrev/2

revapp([], L2, R) :- R = L2.
revapp([X1|L1], L2, R) :-
	revapp(L1, [X1|L2], R).
	

reverse(L, R) :-
	revapp(L, [], R).

%3. Elements occurring in pairs
% in_pair(+List, ?E, ?I):  E is an element of List equal to its
% right neighbour, occurring at position I (numbered from 1).
% | ?- in_pair([1,8,8,3,4,4], E, I).          E = 8, I = 2 ? ;
%                                             E = 4, I = 5 ? ; no
in_pair([X,X|_], X, 1).
in_pair([_|L], E, I) :-
	in_pair(L, E, I0),
	I is I0+1.

%4. Appending three lists
% append(L1, L2, L3, L123):  L123 is the concatenation of L1, L2, and L3.
% | ?- append([1,2,3,4,5], [13,14], [17], L).
% L = [1,2,3,4,5,13,14,17] ? ; no
% | ?- append(L1, L2, L3, [a]).
% L1 = [], L2 = [], L3 = [a] ? ;
% L1 = [], L2 = [a], L3 = [] ? ;
% L1 = [a], L2 = [], L3 = [] ? ; no
% 
% Try to make append/4 run in n + m steps when L1 and L2 are proper lists,
% having n and m elements, respectively.
%append([], [], L3, L123) :- L123 = L3.
%append([], L2 ,[], L123) :- L123 = L2.
%append(L1, [] ,[], L123) :- L123 = L1.
%append([], L2, L3, L123) :- append(L2, L3, L123).
%append(L1, [], L3, L123) :- append(L1, L3, L123).
%append(L1, L2, [], L123) :- append(L1, L2, L123).

%append([], L2 , L3, L123) :- append(L2, L3, L123).
%append([X|L1], L2, L3, [X|L123]) :-
%	append(L1, L2, L3, L123).
	
append(L1, L2, L3, L123):-
	append(L1, T, L123),
	append(L2, L3, T).
	
%5. Appending a list of lists
% append(LL, L):  LL is a proper list whose elements are (possibly open ended) lists.
% L is a concatenation of all the lists in LL.
%| ?- append([[1,a+b,[3,4]],[],[a,b],[c,d]], L).
%L = [1,a+b,[3,4],a,b,c,d] ? ; no
%| ?- append([L1,L2,L3,L4], [a]).
%L1 = [], L2 = [], L3 = [], L4 = [a] ? ;
%L1 = [], L2 = [], L3 = [a], L4 = [] ? ;
%L1 = [], L2 = [a], L3 = [], L4 = [] ? ;
%L1 = [a], L2 = [], L3 = [], L4 = [] ? ; no

append([], L) :- L = [].
append([LL1|LL2], L) :-
    append(LL1, L2, L),
    append(LL2, L2).


%6. Selecting an element from a list
% select(Elem, List, Rest): Elem is an element of List, and removing it from
% List results in list Rest.
%A simplified version of predicate select/3 , which only works in mode select(?E, +L, ?R)
%. i.e. when L is proper, could be defined as follows:
%select(E, L, R) :-
%	append(Front, [E|Back], L),
%	append(Front, Back, R).
%Your task is to define select/3 as a single recursive predicate,
%which produces the results given for all the test cases below:
%| ?- select(1, [2,1,3,1], L).
%L = [2,3,1] ? ; L = [2,1,3] ? ; no
%| ?- select(X, [1,2,3], L).
%L = [2,3], X = 1 ? ;  L = [1,3], X = 2 ? ;   L = [1,2], X = 3 ? ; no
%| ?- select(3, L, [1,2]).
%L = [3,1,2] ? ; L = [1,3,2] ? ; L = [1,2,3] ? ; no
%| ?- select(3, [2|L], [1,2,7,3,2,1,8,9,4]).
%no
%| ?- select(1, [X,2,X,3], L).
%L = [2,1,3], X = 1 ? ; L = [1,2,3], X = 1 ? ; no

select(E, [E|T], T).
select(E, [H|T], [H|V]):-
	select(E,T,V).
%select(E, L, R) :-
%	L = [E|T],
%	R = T.
%select(E, L, R) :-
%	L = [H|T],
%	U = E,
%	select(U, T, V),
%	R = [H|V].
    


end_of_file.