:- module mother.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type persona ---> laura ; rafael ; james.

% El mismo predicado, dos modos distintos:
%   dado el hijo (in) → busca la madre (out): semidet
%   dada la madre (out) → busca hijos (in):   nondet
:- pred mother(persona, persona).
:- mode mother(in, out) is semidet.
:- mode mother(out, in) is nondet.

mother(rafael, laura).
mother(james,  laura).

main(!IO) :-
    ( if mother(rafael, X) then
        io.write_string("La madre de rafael es ", !IO),
        io.write(X, !IO),
        io.nl(!IO)
    else
        io.write_string("Rafael no tiene madre registrada.\n", !IO)
    ).
