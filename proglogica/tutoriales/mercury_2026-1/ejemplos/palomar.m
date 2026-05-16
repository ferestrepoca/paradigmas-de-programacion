:- module palomar.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, string, bool.

% Principio del palomar (pigeonhole principle):
% Si hay más palomas (N) que casilleros (M), al menos un casillero
% alberga más de una paloma.
:- pred pigeonhole(int::in, int::in, bool::out) is det.
pigeonhole(N, M, Result) :-
    ( if N > M then Result = yes else Result = no ).

:- pred print_result(int::in, int::in, bool::in, io::di, io::uo) is det.
print_result(N, M, Result, !IO) :-
    ( if Result = yes then
        io.format(
            "%d palomas en %d casilleros: al menos uno tendrá más de una.\n",
            [i(N), i(M)], !IO)
    else
        io.format(
            "%d palomas en %d casilleros: ninguno tendrá más de una.\n",
            [i(N), i(M)], !IO)
    ).

main(!IO) :-
    pigeonhole(10, 9, R1), print_result(10, 9, R1, !IO),
    pigeonhole(5,  5, R2), print_result(5,  5, R2, !IO),
    pigeonhole(3,  7, R3), print_result(3,  7, R3, !IO).
