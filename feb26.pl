% Idea of sum:
% Think of sum as a one-argument function.
% sum([] , X) :-
% 	X = 0.
sum([], 0).
% sum(L, X) :-
% 	L = [], X = 0.

sum([X|L], S0) :-
	sum(L,S0),
	S0 is X+S0.		% Recursive relational definition.


% cons(H, T, L): L is a list whose head and tail are H and T, resp.
cons(H, T, L) :-
	L = [H|T].
cons1(H, T, [H|T]).


% Idea of pow:
% pow(+A, +E, ?P): P is A to the power E (E is an integer >= 0.)

pow(A, 0, 1).
pow(A, E, P):-
	E0 is E - 1,
	pow(A, E0, P0),
	P0 is A * P.



% Idea of sum of leaves of a binary tree.
is_tree(leaf(V)) :-
	integer(V).

is_tree(node(Left, Right)) :-
	is_tree(Left),
	is_tree(Right).


%% Difference between = and is?
tree_sum(Tree, Value) :-
	Tree = leaf(Value).
tree_sum(node(Left, Right), S) :-
	tree_sum(Left, S1),
	tree_sum(Right, S2),
	S is S1+S2.

end_of_file.