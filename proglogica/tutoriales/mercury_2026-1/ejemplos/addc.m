:- module addc.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, int.

% Predicado implementado en C mediante foreign_proc.
% Las anotaciones indican al compilador las garantías del código externo:
%   will_not_call_mercury — el código C no llama de vuelta a Mercury
%   promise_pure           — el predicado es referencialmente transparente
%   thread_safe            — seguro para ejecución concurrente
:- pred add(int::in, int::in, int::out) is det.
:- pragma foreign_proc("C",
    add(A::in, B::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = A + B;
").

main(!IO) :-
    io.read_line_as_string(ResultA, !IO),
    (
        ResultA = ok(LineA),
        ( if string.to_int(string.strip(LineA), A) then
            io.read_line_as_string(ResultB, !IO),
            (
                ResultB = ok(LineB),
                ( if string.to_int(string.strip(LineB), B) then
                    add(A, B, Sum),
                    io.write_string("La suma es: ", !IO),
                    io.write_int(Sum, !IO),
                    io.nl(!IO)
                else
                    io.write_string("Error: B no es un entero válido.\n", !IO)
                )
            ;
                ResultB = eof,
                io.write_string("Error: fin de entrada al leer B.\n", !IO)
            ;
                ResultB = error(ErrB),
                io.format("Error al leer B: %s\n",
                          [s(io.error_message(ErrB))], !IO)
            )
        else
            io.write_string("Error: A no es un entero válido.\n", !IO)
        )
    ;
        ResultA = eof,
        io.write_string("Error: fin de entrada al leer A.\n", !IO)
    ;
        ResultA = error(ErrA),
        io.format("Error al leer A: %s\n",
                  [s(io.error_message(ErrA))], !IO)
    ).
