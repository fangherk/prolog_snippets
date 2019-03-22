has_parent(Herrick, Min).
has_parent(Jenny, Min).
has_parent(Mum, GrandmaM).
has_parent(Herrick, YiGan).
has_parent(Jenny, YiGan).


has_grandparent(GC, GP) :-
	has_parent(GC, P),
	has_parent(P, GP).

end_of_file.


