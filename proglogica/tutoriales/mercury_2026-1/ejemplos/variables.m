:- module variables.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, float, string, list.

main(!IO) :-
    % Declaración de variables de distintos tipos.
    % En Mercury, el tipo se infiere del primer uso — no puede cambiar.
    IntVar    = 42,
    FloatVar  = 3.14,
    StringVar = "Hola, Mercury",
    ListVar   = [1, 2, 3, 4, 5],

    io.write_string("Valor de IntVar: ",    !IO),
    io.write_int(IntVar,                    !IO),
    io.nl(!IO),

    io.write_string("Valor de FloatVar: ",  !IO),
    io.write_float(FloatVar,                !IO),
    io.nl(!IO),

    io.write_string("Valor de StringVar: ", !IO),
    io.write_string(StringVar,              !IO),
    io.nl(!IO),

    io.write_string("Valores de ListVar: ", !IO),
    io.write_list(ListVar, ", ", io.write_int, !IO),
    io.nl(!IO).
