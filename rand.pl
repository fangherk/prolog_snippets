list_concat([], [], L3) :- L3 = [].
list_concat([], L2, L3) :-
	L3 = L2.
list_concat([X1|L1], L2, L3) :-
	list_concat(L1, [X1|L2], L3).

concatenated([], L, L).
concatenated([X|L1], L2, [X|L3]) :-
	concatenated(L1, L2, L3).

treble(L, LLL) :-
	concatenated(L, LL, LLL),
	concatenated(L, L, LL).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(fdbg)).
:- use_module(library(between)).

% use fdbg_on.

send(SEND, MORE, MONEY) :-
	Vars=[S,E,N,D,M,O,R,Y],
	domain(Vars, 0, 9), S#\=0, M#\=0,
	all_different(Vars),
	SEND#=1000*S + 100 * E+ 10*N+D,
	MORE#=1000*M + 100 * O+ 10*R+E,
	MONEY#=10000*M+ 1000*O+ 100*N+ 10*E+Y,
	SEND+MORE#=MONEY,
	labeling([], Vars).

% queens_gt(N, Qs): Qs is a good placement of N queens on an NxN chessboard.
queens_gt(N, Qs):-
	placement(N, N, Qs), safe(Qs).
% placement(N, M, P): P is a list of length M, each element of P is between 1 and N.
placement(_, 0, []).
placement(N, M, [X|P]) :-
	M > 0, M1 is M-1, between(1, N, X), placement(N, M1, P).
% safe(Qs): In placement Q, no pair of queens attack each other.
safe([]).
safe([Q|Qs]):-
	no_attack(Q, Qs, 1), safe(Qs).
% no_attack(Q, Qs, I): Q is the placement of the queen in row k,
% Qs lists the placements of queens in rows k+I, k+I+1, ...
% Queen in row k does not attack any of the queens listed in Qs.
no_attack(_, [], _).
no_attack(X, [Y|Ys], I):-
	no_threat(X, Y, I), J is I+1, no_attack(X, Ys, J).
% no_threat(X, Y, I): queens placed in column X of row k and in column Y of row k+I
% do not attack each other.
no_threat(X, Y, I) :-
	Y =\= X, Y =\= X-I, Y =\= X+I.

length(L, 3), domain(L, 1, 100), count(3, L, #=, _C),
count(2, L, #>, _C), _C #> 0, labeling([], L).


test13(L) :-
	length(L, 4),
	automaton(L, [source(s0),sink(s2)],[arc(s0,1,s1),arc(s1,1,s1),arc(s1,2,s2),arc(s2,2,s2)]),
	labeling([], L).

zero([]).
zero([H|T]) :-
	H = 0; zero(T).



end_of_file.