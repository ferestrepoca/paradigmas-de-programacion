:- module family.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, solutions.

:- type person ---> ada ; bob ; dan ; ema ; fay ; joe.

:- pred female(person).
:- mode female(in)  is semidet.
:- mode female(out) is multi.
female(ada).
female(ema).
female(fay).

:- pred male(person).
:- mode male(in)  is semidet.
:- mode male(out) is multi.
male(bob).
male(dan).
male(joe).

:- pred parent(person, person).
:- mode parent(in,  in)  is semidet.
:- mode parent(in,  out) is nondet.
:- mode parent(out, in)  is nondet.
:- mode parent(out, out) is multi.
parent(ada, dan).
parent(bob, dan).
parent(dan, fay).
parent(ema, fay).
parent(dan, joe).
parent(ema, joe).

:- pred father(person, person).
:- mode father(in,  in)  is semidet.
:- mode father(in,  out) is nondet.
:- mode father(out, in)  is nondet.
:- mode father(out, out) is nondet.
father(P, C) :- parent(P, C), male(P).

:- pred mother(person, person).
:- mode mother(in,  in)  is semidet.
:- mode mother(in,  out) is nondet.
:- mode mother(out, in)  is nondet.
:- mode mother(out, out) is nondet.
mother(P, C) :- parent(P, C), female(P).

:- pred grandparent(person, person).
:- mode grandparent(in,  in)  is semidet.
:- mode grandparent(in,  out) is nondet.
:- mode grandparent(out, in)  is nondet.
:- mode grandparent(out, out) is nondet.
grandparent(PP, C) :- parent(PP, P), parent(P, C).

main(!IO) :-
    % solutions/2 recolecta TODAS las soluciones de un closure nondet en una lista.
    solutions(
        (pred(PP::out) is nondet :- grandparent(PP, fay)),
        GPs),
    (
        GPs = [],
        io.write_string("fay no tiene abuelos registrados.\n", !IO)
    ;
        GPs = [_ | _],
        io.write_string("Abuelos de fay: ", !IO),
        io.write(GPs, !IO),
        io.nl(!IO)
    ).
