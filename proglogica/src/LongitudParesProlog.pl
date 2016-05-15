longitud([], 0).
longitud([_|Xs], N) :- 
    longitud(Xs, N1), N is N1 + 1.

pares([], []).
pares([X|Xs], L) :- X mod 2 =:= 0, pares(Xs, Laux), append([X], Laux, L).
pares([X|Xs], L) :- X mod 2 =\= 0, pares(Xs, Laux), L = Laux.