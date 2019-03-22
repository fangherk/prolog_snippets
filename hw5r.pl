% tree_node_count(+Tree, ?N): Binary tree Tree has N nodes.
tree_node_count(leaf(_), 0).
tree_node_count(node(L, R), N) :-
	tree_node_count(L, N1),
	tree_node_count(R, N2),
	N is N1 + N2 + 1.

% increment_tree(+Tree0, ?Tree): Tree is obtained from binary tree
% Tree0 by incrementing each leaf value by 1.
increment_tree(leaf(A), Tree) :-
	A1 is A + 1,
	Tree = leaf(A1).
increment_tree(node(L, R), Tree) :-
	increment_tree(L, Tree1),
	increment_tree(R, Tree2),
	Tree = node(Tree1, Tree2).

% tree_rightmost_value(+Tree, ?Value): Tree is a binary tree and
% the integer in its rightmost leaf is Value.
tree_rightmost_value(leaf(Val), Val).
tree_rightmost_value(node(_, R), Val) :-
	tree_rightmost_value(R, Val1),
	Val = Val1.

% tree_leaf_value(+Tree, +V): V is present as a leaf value in Tree.
tree_leaf_value(leaf(Val), Val).
tree_leaf_value(node(L, R), Val):-
	tree_leaf_value(L, Val);
	tree_leaf_value(R, Val).
	
% increment_list(+L0, ?L): L is a list of numbers obtained
% from L0 by incrementing each element by 1.
% You can assume that L0 is given and is a list of numbers
increment_list([], []).
increment_list([H|T], [H1|T1]):-
	H1 is H + 1,
	increment_list(T, T1).


% list_last(+List, ?V): List is a list whose last element is V
list_last([E], E).
list_last([_|T], L):-
	list_last(T, L).

% list_element(+List, +V): V is present as an element in the list List.
list_element([H|T], V) :-
	H = V ;
	list_element(T, V).

% list_suffix(+L0, -L): L is a suffix of L0
list_suffix([], []).
list_suffix([H|T], Suff) :-
	Suff = [H|T];
	list_suffix(T, Suff).

% list_concat(+L1, +L2, -L3): L3 is the concatenation of lists L1 and L2.
list_concat([], L2, L2).
list_concat([H1|T1], L2, [H1|L3]) :-
	list_concat(T1, L2, L3).