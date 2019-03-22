hParent(herk, mum).
hParent(herk, dud).
hParent(jen, mum).
hParent(jen, dud).
hParent(mum, grandmumM).
hParent(mum, grandadM).
hParent(dud, grandmumD).
hParent(dud, grandadD).


hGP(Ch, G) :-
	hParent(Ch, P),
	hParent(P, G).

end_of_file.