:- module factorial.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- func factorial(int) = int.
factorial(0) = 1.
factorial(N) = N * factorial(N - 1) :- N > 0.

main(!IO) :-
    io.format("factorial(0)  = %d\n", [i(factorial(0))],  !IO),
    io.format("factorial(5)  = %d\n", [i(factorial(5))],  !IO),
    io.format("factorial(10) = %d\n", [i(factorial(10))], !IO).
