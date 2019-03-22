% orderedN(+L): The list of numbers L is ordered.
ordered0([]).
ordered0([_]).
ordered0([X,Y|L]) :-
	X < Y, ordered0([Y|L]).


ordered1([]).
ordered1([_]) :- !.
ordered1([X,Y|L]) :-
	ordered1(X, Y, L).
ordered1(_, _, []) :- !.
ordered1(X, Y, [Z|M]) :-
	X < Y, ordered1(Y, Z, M).
	

ordered2([]).
ordered2([X|L]):-
	ordered2(L,X).

ordered2([], _).
ordered2([Y|L], X) :-
	X < Y, ordered2(L, Y).



p0(1, 2).
p0(X, X) :- \+ X=1.

p1(1, 2) :- !.
p1(X, X) :- \+ X=1.

p2(1, 2) :- !.
p2(X, X).

p3(1, Y):- !, Y = 2.
p3(X, X).

%increment_list(+L0, ?L): List is a list of numbers obtained
%from the list of numbers L0 by incrementing each element by 1

increment_list(L0, L):-
	findall(Y,(member(X,L0), Y is X+1), L).

pos_elems(L0, L):-
	findall(X,(member(X,L0), X>0), L).


emp(a,b).
emp(a,c).
emp(b,c).
emp(b,d).
employees(Er, Es):-
	findall(Ee,emp(Er,Ee), Es).


employees1(Er, Es):-
	bagof(Ee,emp(Er,Ee), Es).

employee_count(Er, C):-
	bagof(Ee,emp(Er,Ee), Es),
	length(Es, C).

employee_counts(RCL):-
	bagof(Ee,emp(Er,Ee), Es),
	length(Es, C),
	append([Er-C], [] ,RCL).


end_of_file.