% in  = ya instanciado (entrada)
% out = no instanciado (salida)
% di  = estado destruible (E/S entrada)
% uo  = estado único (E/S salida)

:- pred suma(int::in, int::in, int::out) is det.
suma(X, Y, Z) :- Z = X + Y.

% Azúcar sintáctico: func declara in/in -> out automáticamente
:- func suma_f(int, int) = int.
suma_f(X, Y) = X + Y.

% Predicado semidet: puede fallar si X no está en la lista
:- pred miembro(T::in, list(T)::in) is semidet.
miembro(X, [X|_]).
miembro(X, [_|T]) :- miembro(X, T).
