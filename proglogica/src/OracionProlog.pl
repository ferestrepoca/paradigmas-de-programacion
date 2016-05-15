oracion(O) :- sintagma_nominal(SN),
sintagma_verbal(SV),
append(SN,SV,O).

sintagma_nominal(SN) :- nombre(SN).

sintagma_nominal(SN) :- articulo(A),
nombre(N),
append(A,N,SN).

sintagma_verbal(SV) :- verbo(V),
sintagma_nominal(SN),
append(V,SN,SV).

articulo([el]).
nombre([gato]).
nombre([perro]).
nombre([pescado]).
nombre([carne]).
verbo([come]).