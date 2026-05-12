:- module ordenamiento.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

:- pred insertar(int::in, list(int)::in, list(int)::out) is det.
insertar(X, [],     [X]).
insertar(X, [H|T],  [X,H|T]) :- X =< H.
insertar(X, [H|T],  [H|T1])  :- X > H, insertar(X, T, T1).

:- pred insertion_sort(list(int)::in, list(int)::out) is det.
insertion_sort([], []).
insertion_sort([H|T], Sorted) :-
    insertion_sort(T, ST),
    insertar(H, ST, Sorted).

main(!IO) :-
    Lista = [64, 25, 12, 22, 11, 90, 3],
    insertion_sort(Lista, Ordenada),
    io.format("Original: %s\n",  [s(string(Lista))],     !IO),
    io.format("Ordenada: %s\n",  [s(string(Ordenada))],  !IO).
