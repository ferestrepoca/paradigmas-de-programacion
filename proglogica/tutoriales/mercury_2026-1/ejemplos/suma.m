:- module suma.
:- interface.
:- import_module io.

:- pred suma(int::in, int::in, int::out) is det.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, string, list.

suma(X, Y, Resultado) :- Resultado = X + Y.

main(!IO) :-
    io.read_line_as_string(Result1, !IO),
    (
        Result1 = ok(Line1),
        ( if string.to_int(string.strip(Line1), X) then
            io.read_line_as_string(Result2, !IO),
            (
                Result2 = ok(Line2),
                ( if string.to_int(string.strip(Line2), Y) then
                    suma(X, Y, Resultado),
                    io.format("La suma de %d y %d es %d\n",
                              [i(X), i(Y), i(Resultado)], !IO)
                else
                    io.write_string(
                        "Error: el segundo valor no es un número válido.\n", !IO)
                )
            ;
                Result2 = eof,
                io.write_string(
                    "Error: fin de entrada al leer el segundo número.\n", !IO)
            ;
                Result2 = error(Err2),
                io.format("Error al leer el segundo número: %s\n",
                          [s(io.error_message(Err2))], !IO)
            )
        else
            io.write_string(
                "Error: el primer valor no es un número válido.\n", !IO)
        )
    ;
        Result1 = eof,
        io.write_string(
            "Error: fin de entrada al leer el primer número.\n", !IO)
    ;
        Result1 = error(Err1),
        io.format("Error al leer el primer número: %s\n",
                  [s(io.error_message(Err1))], !IO)
    ).
