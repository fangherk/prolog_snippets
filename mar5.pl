% 1. Naive reverse
% Query -> nrev(L,R).

nrev([], R) :-
	R = [].
nrev([X1|L1], R) :-
	nrev(L1, R1),
	append(R1, [X1], R).

% 2. Reverse Append
% Query -> revapp([a,b,c], [d,e], L).
revapp([], L1, L2) :- L2 = L1.
revapp([X1|L1], L2, L3) :-
	revapp(L1, [X1|L2], L3).

reverseH(L1, L2) :-
	revapp(L1,[],L2).
% 3. In-Pair

end_of_file.

