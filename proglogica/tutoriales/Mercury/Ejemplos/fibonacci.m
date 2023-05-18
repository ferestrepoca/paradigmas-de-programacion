:- module fibonacci.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- pred fib(int::in, int::out) is det.

fib(N, X) :-
    ( if N =< 2
      then X = 1
      else fib(N - 1, A), fib(N - 2, B), X = A + B
    ).

main(!IO) :-
    fib(3, X),
    io.write_string("fib(3, ", !IO),
    io.write_int(X, !IO),
    io.write_string(")\n", !IO).