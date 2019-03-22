:- use_module(library(clpfd)).
:- use_module(library(lists)).

% q(+A, ?B, ?C, ?D): A, B, C, D are positive integers, A=B+C+D, 2* B<C, 2 * C<D.
% q/4 should not perform labeling.
q(A, B, C, D) :-
	domain([B,C,D], 1, sup),
	A #= B+C+D,
	2 *B #< C,
	2 *C #< D.

% sudoku_simple(?Matrix, +N): Matrix is an NxN matrix, containing integers between 1 and N.
% In each row and in each column, the numbers are pairwise different. sudoku_simple/2 should
% not perform labeling.
sudoku_simple(Matrix, N) :-
	length(Matrix, N),                 % Check size of matrix
	length(TMatrix, N),                % Check size of transpose matrix
	transpose(Matrix, TMatrix),        % Transpose the matrix
	append(Matrix, Vars),              % Add random variables 
	domain(Vars, 1, N),                % Keep them from 1-N
	maplist(all_distinct, Matrix),     % Check that the rows are distinct
	maplist(all_distinct, TMatrix).    % Check that the cols are distinct
	

% checkin(+L, +Limit): Checks if C = A+B for the given list.
checkin([A,B,C|D]):-
	C #= A + B,
	( D = [] 
	; checkin([B,C|D])
	).

% fibonacci(+N, +Limit, ?L): N and Limit are positive integers; L is a list of length N, all
% elements of L are integers from the interval [1,Limit], and for any three consecutive
% elements A, B, C of L, the equation C=A+B holds. fibonacci/3 should not perform labeling
fibonacci(N, Limit, L) :-
	N #> 0, Limit #> 0,
	length(L, N),
	domain(L, 1, Limit),
	checkin(L).
	
% p(?A, ?B): if dividing A by 3 gives remainder 1, then B is even, otherwise B is odd. p/2
% should not perform labeling.
p(A, B) :-
	(A mod 3 + B) mod 2 #= 1.

% itemLists(+A, ?B, ?C, ?D):- A is a list of items, B is a list of A's first elements in the
% item, C is a list of  A's second elements in items, and D is a list of A's third
% elements of item
itemLists([], [], [], []).
itemLists([item(A,B,C)|L], [A|L1], [B|L2], [C|L3]):-
	itemLists(L, L1, L2, L3).



% create_output(+A, +B, ?C) :- A is list, B contains binary inputs, and C represents the elements in
% A such that the corresponding binary output at the index in B is 1 

create_output([], [], []).
create_output([X|L1], [1|Vars], [X|L2]) :-
	create_output(L1, Vars, L2).
create_output([_|L1], [0|Vars], L2) :-
	create_output(L1, Vars, L2).
% knapsack(+Items, +MaxWeight, +MinValue, ?Selected): Items is a list of items of the form
% item(Id,Weight,Value), where Id, Weight, and Value are given positive integers; Id is a
% unique identifier of the item, whereas Weight and Value specify its weight and value.
% MaxWeight and MinValue are given positive integers specifying the capacity of the knapsack
% and the minimum total value of the items to be selected, respectively. Selected should be a
% list of the identifiers of the selected items, in the same order as they appear in Items.
% knapsack/4 should include labeling
knapsack(Items, MaxWeight, MinValue, Selected):-
	length(Items, N),
	length(Vars, N),
	domain(Vars, 0, 1),
	itemLists(Items, X, Y, Z),
	scalar_product(Y, Vars, #=<, MaxWeight),
	scalar_product(Z, Vars, #>=, MinValue),
	create_output(X, Vars, Selected),
	labeling([], Vars).




end_of_file.