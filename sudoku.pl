:- use_module(library(lists)).
:- use_module(library(between)).

% unique(+List): Returns true if a list is unique or not fully filled in,
% otherwise return false
unique([]).
unique([X|Y]) :-
	( member(X, Y),
	  X \= 0 -> false
	; unique(Y)
	).

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


% consistent(+SGrid): For all areas of the sudoku grid Sgrid it holds
% that all positive integers in the area are distinct
consistent(SGrid) :-
	maplist(unique, SGrid),    % Handle the rows
	transpose(SGrid, TSGrid),  % Generate the columns
	maplist(unique, TSGrid),   % Handle the columns
	subgrids(SGrid, SGSGrid),  % Generate the subgrids
	maplist(unique, SGSGrid).  % Handle the sub-grids.

% noZero(+List): Returns true if a list has no zero, otherwise return false.
noZero([]).
noZero([H|T]) :-
	H \= 0, noZero(T).

% zero(+List): Returns true if a list has a zero, otherwise return false.
zero([]).
zero([H|T]) :-
	H = 0; zero(T).

% refineList(+List0, -NList): Modifies the first 0 in the list and
% replaces it with a positive element
% refineList(_, [], []).
refineList(Y, [H|T], NList) :-
	( H = 0 -> NList = [Y|T]
	; refineList(Y, T, NList0),
	  NList = [H|NList0]
	).

% refineLoL(+List0, -Nlist): Modifies the first 0 in a list of lists
% and replaces it with some number
refineLoL(N, [HL|TL], NL) :-
	(refineList(N, HL, NHL) -> NL = [NHL|TL]
	;refineLoL(N, TL, NL0),
	 NL = [HL|NL0]
	).
	
% sudoku0(+Grid0, ?Grid): Grid is a complete refinement of the Sudoku grid Grid0
sudoku0(Grid0, Grid) :-
	( maplist(noZero, Grid0) -> consistent(Grid0), Grid = Grid0
	; length(Grid0, NN),
	  refineLoL(N, Grid0, NGrid),
	  between(1, NN, N),
	  consistent(NGrid),
	  sudoku0(NGrid, Grid)
	).
	
% sudoku0(+Grid0, ?Grid): Grid is a complete refinement of the Sudoku grid Grid0 
sudoku(Grid0, Grid) :-
	sudoku0(Grid0, Grid).


end_of_file.