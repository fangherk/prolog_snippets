hP(a, b).             % +hP(a, b).              (1)
hP(b, c).             % +hP(b, c).              (2)
hP(b, d).             % +hP(b, d).              (3)
hP(d, e).             % +hP(d, e).              (4)

hGP(Ch, G) :-         % +hGP(Ch, G)
         hP(Ch, P),   %    -hP(Ch, P) 
         hP(P, G).    %    -hP(P, G).           (5)

end_of_file.


| ?- hGP(b, GP).        % -hGP(a, GP).          
| ?- hGP(GC, e).        % -hGP(GC, d).        
| ?- hGP(_GC, GP).      % -hGP(GC, GP.  
| ?- hGP(GC, _GP).      % -hGP(GC, GP).

