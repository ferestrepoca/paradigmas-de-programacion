:- module figuras.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module float, math.

:- type figura
    --->    circulo(float)
    ;       rectangulo(float, float)
    ;       triangulo(float, float, float).

:- func area(figura) = float.
area(circulo(R))           = math.pi * R * R.
area(rectangulo(B, H))     = B * H.
area(triangulo(A, B, C))   = sqrt(S * (S-A) * (S-B) * (S-C)) :-
    S = (A + B + C) / 2.0.

:- func nombre(figura) = string.
nombre(circulo(_))          = "Circulo".
nombre(rectangulo(_, _))    = "Rectangulo".
nombre(triangulo(_, _, _))  = "Triangulo".

:- pred mostrar(figura::in, io::di, io::uo) is det.
mostrar(F, !IO) :-
    io.format("%s: area = %.4f\n", [s(nombre(F)), f(area(F))], !IO).

main(!IO) :-
    mostrar(circulo(5.0),          !IO),
    mostrar(rectangulo(4.0, 6.0),  !IO),
    mostrar(triangulo(3.0, 4.0, 5.0), !IO).
