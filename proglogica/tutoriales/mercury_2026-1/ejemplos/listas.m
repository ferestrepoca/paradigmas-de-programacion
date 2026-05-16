:- module listas.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

:- func mi_longitud(list(T)) = int.
mi_longitud([]) = 0.
mi_longitud([_|T]) = 1 + mi_longitud(T).

% Inversión con acumulador (eficiente O(n))
:- func invertir(list(T)) = list(T).
invertir(L) = aux(L, []).

:- func aux(list(T), list(T)) = list(T).
aux([], Acc)    = Acc.
aux([H|T], Acc) = aux(T, [H|Acc]).

:- func concatenar(list(T), list(T)) = list(T).
concatenar([], L)    = L.
concatenar([H|T], L) = [H | concatenar(T, L)].

main(!IO) :-
    Lista = [1, 2, 3, 4, 5],
    io.format("Lista:       %s\n", [s(string(Lista))],              !IO),
    io.format("Longitud:    %d\n", [i(mi_longitud(Lista))],          !IO),
    io.format("Invertida:   %s\n", [s(string(invertir(Lista)))],     !IO),
    io.format("Concat [6..8]:%s\n",[s(string(concatenar(Lista,[6,7,8])))], !IO).
