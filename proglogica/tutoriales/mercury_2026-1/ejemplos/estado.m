:- module estado.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

% Cuenta regresiva: el estado (N) se pasa por recursión
:- pred cuenta_regresiva(int::in, io::di, io::uo) is det.
cuenta_regresiva(0, !IO) :-
    io.write_string("Despegue!\n", !IO).
cuenta_regresiva(N, !IO) :-
    N > 0,
    io.format("  %d...\n", [i(N)], !IO),
    cuenta_regresiva(N - 1, !IO).

% Suma acumulativa: el acumulador es el estado
:- pred sumar_lista(list(int)::in, int::in, int::out) is det.
sumar_lista([], Acc, Acc).
sumar_lista([H|T], Acc, Total) :-
    NuevoAcc = Acc + H,
    sumar_lista(T, NuevoAcc, Total).

% Producto con acumulador
:- pred producto_lista(list(int)::in, int::in, int::out) is det.
producto_lista([], Acc, Acc).
producto_lista([H|T], Acc, Prod) :-
    producto_lista(T, Acc * H, Prod).

main(!IO) :-
    io.write_string("=== Cuenta regresiva ===\n", !IO),
    cuenta_regresiva(5, !IO),
    io.nl(!IO),

    Lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    sumar_lista(Lista, 0, Suma),
    producto_lista(Lista, 1, Prod),
    io.format("=== Lista 1..10 ===\n", [], !IO),
    io.format("Suma:     %d\n", [i(Suma)], !IO),
    io.format("Producto: %d\n", [i(Prod)], !IO).
