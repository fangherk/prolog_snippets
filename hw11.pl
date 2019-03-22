:- use_module(library(clpfd)).
:- use_module(library(lists)).

% optnum_helper(L, N, K):- N counts the number of optima so far
optnum_helper(L, 0) :-
	N #< 3,
	length(L, N).
optnum_helper([A,B,C|D], K) :-
	K #> 0 #<=> S,                  % Is K greather than 0?
	((B #> A) #/\ (B #> C)) #<=> X, % max
	((B #< A) #/\ (B #< C)) #<=> Y, % min
	X #\/ Y #<=> Z,                 % optima
	(S #/\ Z) #\/ ((S #\=1) #/\ (Z#=0)),          % K greater then use optima, otherwise, finish?
	(S #/\ K0 #= K - 1) #\/ K0#=K, % choose what goes next?
	optnum_helper([B,C|D], K0).


% optnum(+L, ?K): the # of local optima in list L is K and the elements of L are pairwise non-equal.
% It can be assumed that L is proper (i.e., not open-ended). The elements of L as well as K are
% FD- variables or integers. optnum/2 should not perform any labeling nor create any choice points
optnum(L, K) :-
	all_distinct(L),
	optnum_helper(L, K).
	
% visnum_helper(L, CMax, K):- N counts the number of left_visible elems in L where CMax is the current max
visnum_helper([A], CMax,  N):-
	A #> CMax #<=>N.
	
visnum_helper([A,B|C], CMax, K):-
	A #> CMax #<=> Inc,
	NCMax #= max(A,CMax),
	visnum_helper([B|C], NCMax, K0),
	K #= K0 + Inc.
 
% visnum(+L, ?K) : the number of elements in list L that are visible from the left is K.
% It can be assumed that L is proper (i.e., not open-ended). The elements of L as well as K are
% FD-variables or integers. visnum/2 should not perform labeling nor create any choice points. 
visnum(L, K) :-
	minimum(M, L),
	M0 #= M - 1,
	visnum_helper(L, M0, K).


%is_diff (L, X, N): the elements of L are checked with X and N returns the count of pairwise different vals
is_diff([Y|Z], X, N):-
        Z = [], X #\= Y #<=> N. 
is_diff([Y|Z], X, NDiff):-
	Z \= [],
	(C #= 0 #/\ (X #= Y)) #\/ (C #= 1 #/\ (X #\= Y)),
	is_diff(Z, X, B0),
	NDiff #= B0 + C.
	
%check_diff (L, B): B counts the number of unique elements of list L
check_diff([_], 0).
check_diff(L, B):-
	L = [X,Y|Z],
	is_diff([Y|Z], X, NDiff),
	length(L, Len),
	Len #= NDiff + 1 #<=> C0,
	check_diff([Y|Z], B0),
	B #= C0 + B0.
	
% otherwise B=0. It can be assumed that L is proper (i.e., not open-ended). B and the elements of L
% are FD-variables or integers
% alldifferent_reif/2 should not perform labeling nor create any choice points
alldifferent_reif(L, B):- 
	check_diff(L, NB),
	length(L, Len),
	(Len #= NB+1 #/\ B #= 1) #\/ (Len#\=NB+1 #/\ B#=0).
	
end_of_file.