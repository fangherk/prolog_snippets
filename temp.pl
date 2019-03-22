
sum(L, S) :- L = [], S = 0.  % Base Case.
sum([X|L], S) :- sum(L,T), S is X+T.  % Recursive relational definition.

sum([], 0).
