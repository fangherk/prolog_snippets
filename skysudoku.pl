:- use_module(library(clpfd)).
:- use_module(library(lists)).

% split(+N, +List, -Front, -Back): Front is the list containing the first
% N elements of List, and Back is the list of the remaining elements of
% List. N can be assumed to be a given integer.
split(0, List, [], List).
split(N, [H|T], Front, Back) :-
	N1 is N-1,
	split(N1, T, TFront, Back),
	Front = [H|TFront].

% chop(+N, +List, -LofLists): LofLists is a list whose elements are
% nonempty lists, such that the concatenation of these results in List.
% All elements of LofLists, except for the last, have length N, the
% length of the last should be between 1 and N (inclusive).
chop(N, List, LL) :-
	(   length(List, T), T >= N -> split(N, List, Front, Back),
	    chop(N, Back, LL0),
	    LL = [Front|LL0]
	;   List = [] -> LL = List
	;   LL = [List]
	).

% subgrids(+X, -Y): Y is a list of subgrids of X 
subgrids(X, Y) :-
	length(X, LL),
	K is integer(sqrt(LL)),  % Obtain the length of the subgrid row/col.
	chop(K, X, Z),
	maplist(transpose, Z, ZZ),
	maplist(append, ZZ, YY),
	maplist(chop(LL), YY, TT),
	append(TT, Y). 

% gn(+H): H is a given number hint.
gn(g(_,_,_)).

% vc(+H): H is a visible count hint.
vc(v(_,_,_)).

% extract_info(+H, -K, -GN, -VC): Given the hints, K is the grid size, GN is a list of given number hints, and VC is a list of visible count hints. 
extract_info(ss(K,L), K, GN, VC) :-
	include(gn, L, GN),  % Filters the list for gs
	include(vc, L, VC).  % Filters the list for vs

% sudoku_simple(?Matrix, +N): Matrix is an NxN matrix, containing integers between 1 and N.
% In each row and in each column, the numbers are pairwise different. sudoku_simple/2 should
% not perform labeling.
sudoku_simple(Matrix, N) :-
	NSquared is N*N,
	length(Matrix, NSquared),                 % Check size of matrix
	length(TMatrix, NSquared),                % Check size of transpose matrix
	transpose(Matrix, TMatrix),        % Transpose the matrix
	subgrids(Matrix, SMatrix),
	append(Matrix, Vars),              % Add random variables 
	domain(Vars, 1, NSquared),                % Keep them from 1-N
	maplist(all_distinct, Matrix),     % Check that the rows are distinct
	maplist(all_distinct, TMatrix),
	maplist(all_distinct, SMatrix).    % Check that the cols are distinct

% given_nums(+In, +Hints, -Out): Out is a matrix that adds the given constraints from Hints to the input matrix 
given_nums(Out, [], Out).
given_nums(In, [g(N,R,C)|T], Out) :-
	nth1(R, In, Row),
	element(C, Row, N),
	given_nums(In, T, Out).

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

% north_vc(+In, +Hints, -Out): Out is a matrix that takes into account the north given visible counts from Hints and applies these constraints on the matrix In 
north_vchelper(Out, [], Out).
north_vchelper(In, [v(_, Dir, _)|T], Out) :-
	Dir \= n,
	north_vchelper(In, T, Out).
north_vchelper(In, [v(V, n, RC)|T], Out) :-
	nth1(RC, In, Col),
	visnum(Col,V),
	north_vchelper(In, T, Out).

% north_vc(+In, +Hints, -Out): Out is a matrix that takes into account the east given visible counts from Hints and applies these constraints on the matrix In 
north_vc(In, Hints, Out) :-
	transpose(In, TIn),
	north_vchelper(TIn, Hints, TOut),
	transpose(TOut, Out).

% south_vc(+In, +Hints, -Out): Out is a matrix that takes into account the south given visible counts from Hints and applies these constraints on the matrix In 
south_vchelper(Out, [], Out).
south_vchelper(In, [v(_, Dir, _)|T], Out) :-
	Dir \= s,
	south_vchelper(In, T, Out).
south_vchelper(In, [v(V, s, RC)|T], Out) :-
	nth1(RC, In, Row),
	visnum(Row,V),
	south_vchelper(In, T, Out).

% east_vc(+In, +Hints, -Out): Out is a matrix that takes into account the east given visible counts from Hints and applies these constraints on the matrix In 
south_vc(In, Hints, Out) :-
	transpose(In, TIn),
	maplist(reverse, TIn, TRIn),
	south_vchelper(TRIn, Hints, TOut),
	maplist(reverse, TOut, ROut),
	transpose(ROut, Out).

% east_vchelper(+In, +Hints, -Out): Out is a matrix that takes into account the east given visible counts from Hints and applies these constraints on the matrix In 
east_vchelper(Out, [], Out).
east_vchelper(In, [v(_, Dir, _)|T], Out) :-
	Dir \= e,
	east_vchelper(In, T, Out).
east_vchelper(In, [v(V, e, RC)|T], Out) :-
	nth1(RC, In, Row),
	visnum(Row,V),
	east_vchelper(In, T, Out).
% east_vc(+In, +Hints, -Out): Out is a matrix that takes into account the east given visible counts from Hints and applies these constraints on the matrix In 
east_vc(In, Hints, Out) :-
	maplist(reverse, In, TIn),
	east_vchelper(TIn, Hints, TOut),
	maplist(reverse, TOut, Out).

% west_vc(+In, +Hints, -Out): Out is a matrix that takes into account the west given visible counts from Hints and applies these constraints on the matrix In 
west_vc(Out, [], Out).
west_vc(In, [v(_, Dir, _)|T], Out) :-
	Dir \= w,
	west_vc(In, T, Out).
west_vc(In, [v(V, w, RC)|T], Out) :-
	nth1(RC, In, Row),
	visnum(Row,V),
	west_vc(In, T, Out).


% reverseM(WOut, RWOut): Reverse the matrix by rows
reverseM(WOut, RWOut) :-
	maplist(reverse, WOut, RWOut).

% vis_counts(+In, +Hints, -Out): Out is a matrix that takes into account the given visible counts from Hints and applies these constraints on the matrix In 
vis_counts(In, Hints, Out) :-
	west_vc(In, Hints, Out),
	east_vc(In, Hints, Out),
	north_vc(In, Hints, Out),
	south_vc(In, Hints, Out).

% Shave value V off the variable X.
shave_value(V, X) :-
        \+ X = V, !, X in \{V}.
shave_value(_, _).

% Shave off variable X all values within the domain of X.
% Equivalent to cdisj([X=V1,...X=Vn], X), where the domain
% of X is {V1,...,Vn}.
shave_all0(X) :-
	fd_set(X, FD),
	fdset_to_list(FD, L),
	shave_vals(L, X).
% an alternative to the last line:
%	(   foreach(V, L), param(X)
%	do  shave_value(V, X)
%	).

shave_vals([], _).
shave_vals([V|Vs], X) :-
	shave_value(V, X),
	shave_vals(Vs, X).


% Perform shave_all(X), for all elements X of list L.
shave_all_elements(L) :-
	(   foreach(X, L)
	do  shave_all0(X)
	).


% skysudoku(+Hints, ?Sudoku): SS is a solution of the Skyscraper Sudoku puzzle SP
skysudoku(Hints, Sudoku) :-
	extract_info(Hints, Dim, GivenNums, VisCounts),
	sudoku_simple(BasicSudoku, Dim),
	given_nums(BasicSudoku, GivenNums, GSudoku),
	vis_counts(GSudoku, VisCounts, Sudoku),
	append(Sudoku, _Vars),
	shave_all_elements(_Vars),
	labeling([ffc], _Vars). 

end_of_file.


% Test Cases.
| ?- skysudoku(ss(1,[]), SS).
SS = [[1]] ? ;
no
| ?- skysudoku(ss(2,[v(4,n,1),v(4,s,4)]), SS).
SS = [[1,3,2,4],[2,4,1,3],[3,1,4,2],[4,2,3,1]] ? ;
no
| ?- skysudoku(ss(2,[v(4,n,1),g(4,1,4)]), SS).
SS = [[1,3,2,4],[2,4,1,3],[3,1,4,2],[4,2,3,1]] ? ;
SS = [[1,3,2,4],[2,4,1,3],[3,2,4,1],[4,1,3,2]] ? ;
SS = [[1,3,2,4],[2,4,3,1],[3,1,4,2],[4,2,1,3]] ? 
no
| ?- skysudoku(ss(2,[g(4,1,1),v(2,n,1)]), SS).
no
| ?- skysudoku(ss(3,[v(2,n,1),v(6,n,2),v(3,n,3),v(3,n,4),v(1,n,5),
v(3,n,6),v(4,n,7),v(3,n,8),v(2,n,9),v(2,w,1),
v(1,w,2),v(3,w,3),v(3,w,4),v(3,w,5),v(2,w,6),
v(4,w,7),v(3,w,8),v(2,w,9),v(2,s,1),v(2,s,2),
v(1,s,3),v(4,s,4),v(3,s,5),v(3,s,6),v(2,s,7),
v(3,s,8),v(3,s,9),v(2,e,1),v(5,e,2),v(1,e,3),
v(4,e,4),v(2,e,5),v(4,e,6),v(2,e,7),v(3,e,8),
v(4,e,9),g(6,1,4),g(8,2,6),g(2,3,8),g(6,4,3),
g(1,5,5),g(3,6,7),g(5,7,2),g(5,8,4),g(4,9,6)]), SS),
(   member(Row, SS), write(Row), nl, fail
;   nl, fail
).