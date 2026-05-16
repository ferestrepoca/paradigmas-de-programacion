:- module puzzle.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string, solutions.

% Hay una calle con tres casas vecinas de colores distintos.
% En ellas viven personas de distintas nacionalidades con distintas mascotas.
%   - El inglés vive en la casa roja.
%   - El jaguar es la mascota de la familia española.
%   - Los japoneses viven a la derecha del cuidador de caracoles.
%   - El cuidador de caracoles vive a la izquierda de la casa azul.
% ¿Quién se queda con la cebra?

:- type origenes ---> ingles ; japones ; espanol.
:- type colores  ---> rojo ; azul ; verde.
:- type mascotas ---> jaguar ; caracol ; cebra.
:- type casa     ---> casa(origen::origenes, color::colores, mascota::mascotas).

:- pred distinct(casa::in, casa::in) is semidet.
distinct(casa(O1, C1, M1), casa(O2, C2, M2)) :-
    not (O1 = O2 ; C1 = C2 ; M1 = M2).

:- pred fila(list(casa)::out) is nondet.
fila([X, Y, Z]) :-
    casa(X), casa(Y), casa(Z),
    % Los japoneses viven a la derecha del cuidador de caracoles.
    ( X^mascota = caracol <=> Y^origen = japones ),
    ( Y^mascota = caracol <=> Z^origen = japones ),
    % El cuidador de caracoles vive a la izquierda de la casa azul.
    ( Z^color   = azul    <=> Y^mascota = caracol ),
    ( Y^color   = azul    <=> X^mascota = caracol ),
    not X^origen = japones,
    not Z^mascota = caracol,
    distinct(X, Y), distinct(Y, Z), distinct(X, Z).

:- pred casa(casa::out) is nondet.
casa(casa(O, C, M)) :-
    origen(O), color(C), mascota(M),
    % El inglés vive en la casa roja.
    ( O = ingles  <=> C = rojo   ),
    % El jaguar es la mascota de la familia española.
    ( O = espanol <=> M = jaguar ),
    % Los japoneses no cuidan caracoles.
    not (O = japones, M = caracol),
    % El cuidador de caracoles no vive en la casa azul.
    not (M = caracol, C = azul).

:- pred origen(origenes::out) is multi.
origen(ingles). origen(japones). origen(espanol).

:- pred color(colores).
:- mode color(out) is multi.
:- mode color(in)  is det.
color(rojo). color(azul). color(verde).

:- pred mascota(mascotas::out) is multi.
mascota(jaguar). mascota(caracol). mascota(cebra).

main(!IO) :-
    solutions(fila, Soluciones),
    ( if Soluciones = [] then
        io.write_string("Sin solución.\n", !IO)
    else
        list.foldl(
            (pred(L::in, !.IO::di, !:IO::uo) is det :-
                io.print(L, !IO), io.nl(!IO)),
            Soluciones, !IO)
    ).
