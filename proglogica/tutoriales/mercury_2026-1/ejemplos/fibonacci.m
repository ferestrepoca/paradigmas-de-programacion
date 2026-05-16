:- module fibonacci.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- func fib(int) = int.
fib(0) = 0.
fib(1) = 1.
fib(N) = fib(N - 1) + fib(N - 2) :- N > 1.

main(!IO) :-
    io.format("fib(0)  = %d\n",  [i(fib(0))],  !IO),
    io.format("fib(1)  = %d\n",  [i(fib(1))],  !IO),
    io.format("fib(5)  = %d\n",  [i(fib(5))],  !IO),
    io.format("fib(10) = %d\n",  [i(fib(10))], !IO),
    io.format("fib(15) = %d\n",  [i(fib(15))], !IO).
