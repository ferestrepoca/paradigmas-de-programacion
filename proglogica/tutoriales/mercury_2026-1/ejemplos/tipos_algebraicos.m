% Tipo algebraico: arbol binario polimórfico
:- type arbol(T)
    --->    hoja
    ;       nodo(arbol(T), T, arbol(T)).

% Pattern matching exhaustivo (el compilador verifica)
:- func altura(arbol(T)) = int.
altura(hoja) = 0.
altura(nodo(Izq, _, Der)) =
    1 + int.max(altura(Izq), altura(Der)).

:- func contar(arbol(T)) = int.
contar(hoja) = 0.
contar(nodo(Izq, _, Der)) =
    1 + contar(Izq) + contar(Der).
