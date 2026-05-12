/*
  ngtrks es pequeño y verde.
  pgvdrk es un marciano saltarín.
  Todas las criaturas saltarinas son verdes.
  Todas las criaturas pequeñas y saltarinas son marcianas.
  Todas las criaturas verdes y marcianas son inteligentes.

  ¿Cuál de los dos es inteligente?
*/

:- module martians.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- type marciano ---> ngtrks ; pgvdrk.

:- pred small(marciano::out)       is det.
:- pred green(marciano::out)       is multi.
:- pred martian(marciano::out)     is multi.
:- pred jumping(marciano::out)     is det.
:- pred intelligent(marciano::out) is nondet.

small(ngtrks).
green(ngtrks).
martian(pgvdrk).
jumping(pgvdrk).

% Reglas de inferencia
green(X)   :- jumping(X).
martian(X) :- small(X), jumping(X).

intelligent(X) :- green(X), martian(X).

main(!IO) :-
    io.write_string("¿Qué marciano es inteligente?: ", !IO),
    ( if intelligent(X) then
        io.write(X, !IO),
        io.write_string(" es inteligente.\n", !IO)
    else
        io.write_string("No se puede determinar.\n", !IO)
    ).
