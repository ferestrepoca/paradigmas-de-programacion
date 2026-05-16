:- module tipos.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

:- pred ejemplo_predicado(int::in, string::out) is det.
ejemplo_predicado(X, Y) :-
    % Esto sería un error de tipo en Mercury:
    %   Y = X + "hola".
    % Forma correcta: conversión explícita con string.from_int/1.
    Y = string.from_int(X) ++ " hola".

main(!IO) :-
    T = 1,
    % No se puede reasignar ni cambiar el tipo de una variable:
    %   T = "hola",
    %   T = 2,
    _ = T,   % suprimir advertencia de variable no usada
    ejemplo_predicado(42, Resultado),
    io.write_string(Resultado, !IO),
    io.nl(!IO).
