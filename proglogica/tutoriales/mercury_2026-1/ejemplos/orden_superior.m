:- module orden_superior.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

% mi_map: aplica F a cada elemento de la lista
:- func mi_map(func(T) = U, list(T)) = list(U).
mi_map(_, [])     = [].
mi_map(F, [H|T])  = [F(H) | mi_map(F, T)].

% mi_foldl: reduce la lista acumulando con F
:- func mi_foldl(func(T, U) = U, list(T), U) = U.
mi_foldl(_, [], Acc)    = Acc.
mi_foldl(F, [H|T], Acc) = mi_foldl(F, T, F(H, Acc)).

% Funciones concretas para pasar como argumento
:- func doblar(int) = int.
doblar(X) = X * 2.

:- func cuadrado(int) = int.
cuadrado(X) = X * X.

:- func sumar(int, int) = int.
sumar(X, Acc) = X + Acc.

main(!IO) :-
    Lista   = [1, 2, 3, 4, 5],
    Dobles  = mi_map(doblar,   Lista),
    Cuadrs  = mi_map(cuadrado, Lista),
    Suma    = mi_foldl(sumar,  Lista, 0),
    io.format("Original:   %s\n", [s(string(Lista))],   !IO),
    io.format("Dobles:     %s\n", [s(string(Dobles))],  !IO),
    io.format("Cuadrados:  %s\n", [s(string(Cuadrs))],  !IO),
    io.format("Suma total: %d\n", [i(Suma)],             !IO).
