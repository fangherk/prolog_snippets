% 1. Increment each node's value
increment_tree(leaf(A), Tree) :-
	A0 is A + 1,
	Tree = leaf(A0).

increment_tree(node(L,R), Tree) :-
	increment_tree(L, Tree1),
	increment_tree(R, Tree2),
	Tree = node(Tree1, Tree2).


% 2. Find the rightmost leaf value.
tree_rightmost_value(leaf(V), V).
tree_rightmost_value(node(_, R), V0) :-
	tree_rightmost_value(R, V1),
	V0 is V1.

% 3. Finding a leaf value in a tree. The idea is to walk down the left branch. Then, you focus on the right branch to produce your desired result.
tree_leaf_value(leaf(V0), V) :-
	V is V0.
tree_leaf_value(node(L,R), V) :-
	tree_leaf_value(L, V);
	tree_leaf_value(R, V).


% 4. increment_list(+L0, ?L): L is a list of numbers obtained
% from L0 by incrementing each element by 1.
% You can assume that L0 is given and is a list of numbers.

% | ?- increment_list([], L).
% L = [] ? ; no
% | ?- increment_list([1.2,5,2], L).
% L = [2.2,6,3] ? ; no


increment_list([], L) :-
	L = [].
increment_list([F|R], L) :-
	F0 is F + 1,
	increment_list(R, L0),
	L = [F0 | L0].
	

%5. list_last(+List, ?V): List is a list whose last element is V.

% | ?- list_last([], V).
% no
% | ?- list_last([5,1,2,8,7], V).
% V = 7 ? ; no
list_last([X], V) :-
    V = X.
list_last([_|L], V) :-
    list_last(L, V).


%6. Finding a value in a list
% There is a built-in predicate for the task below, but please do not use it.
% % list_element(+List, +V): V is present as an element in the list List.
% | ?- list_element([], x).
% no
% | ?- list_element([a,b,c], a).
% yes
% | ?- list_element([a,b,c,d], c).
% yes
% | ?- list_element([d,c,1,2], 12).
% no
list_element([X|L], V) :-
	V = X;
	list_element(L, V).

% Ensure that your code also works for the input-output mode
% list_element(+List, ?V), by enumerating in V all list elements if V
% is a variable. (The order of the solutions does not matter.)

%7. Suffixes of a list
list_suffix([], V) :-
	V = [].
list_suffix([X|L], V) :-
	V = [X|L];
	list_suffix(L, V).

%8. Concatenating Lists

list_concat([], L2, L3) :-
    L3 = L2.
list_concat([H], L2, L3) :-
    L3 = [H|L2].
list_concat([H|T], L2, L3) :-
    list_concat(T, L2, L0),
    L3 = [H|L0].

end_of_file.
