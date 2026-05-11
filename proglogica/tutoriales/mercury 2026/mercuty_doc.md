PROGRAMACIÓN LÓGICA - MERCURY
Documentación Técnica Completa
Universidad Nacional de Colombia - Lenguajes de Programación
Paradigmas de Programación

Integrantes del grupo: [Nombres aquí]
Fecha: 2025

================================================================================

APARTADO 1: INTRODUCCIÓN (~3 min)
================================================================================

1.1 ¿Qué es la Programación Lógica?
------------------------------------
La programación lógica es un paradigma de programación basado en la lógica formal, específicamente en la lógica de predicados de primer orden. En lugar de describir CÓMO resolver un problema (como en la programación imperativa), el programador describe QUÉ es verdadero sobre el problema.

Principios fundamentales:
- Los programas son conjuntos de hechos y reglas lógicas.
- La ejecución consiste en demostrar que una consulta es verdadera dado el conjunto de hechos y reglas.
- El motor de inferencia (unificación + backtracking) resuelve automáticamente las consultas.
- Se basa en la lógica de Horn clauses (subconjunto de lógica de primer orden).

Características clave:
- Declaratividad: se especifica QUÉ, no CÓMO.
- Unificación: mecanismo central de "igualdad" entre términos.
- Backtracking: exploración automática de alternativas.
- No determinismo: un predicado puede tener múltiples soluciones.

1.2 ¿Qué es Mercury?
---------------------
Mercury es un lenguaje de programación lógico-funcional puro, fuertemente tipado y con polimorfismo paramétrico, diseñado para el desarrollo de software de gran escala y alta confiabilidad.

Historia:
- Creado en 1995 por Fergus Henderson y Thomas Conway en la Universidad de Melbourne, Australia.
- Motivación: Prolog carecía de verificación estática de tipos y tenía problemas de rendimiento para proyectos grandes.
- El nombre "Mercury" fue elegido como el planeta siguiente a Marte (en referencia a "Mars" —Mercury Advanced Research System—, aunque en realidad surgió del contexto universitario).

¿Por qué existe Mercury?
- Prolog, el lenguaje lógico más conocido, tiene limitaciones importantes:
  * Sin sistema de tipos estático → errores difíciles de detectar
  * Sin declaraciones de determinismo → comportamiento impredecible
  * Rendimiento limitado para software de producción
  * Difícil de escalar a proyectos grandes

Mercury resuelve estos problemas añadiendo:
  * Sistema de tipos e instancias estático con inferencia
  * Declaraciones de modos y determinismo
  * Compilación a código nativo eficiente (vía C)
  * Módulos con interfaces bien definidas
  * I/O completamente puro (sin efectos secundarios implícitos)

Posición en el ecosistema:
- Es un lenguaje de programación lógica de "segunda generación"
- Combina la expresividad de la programación lógica con la seguridad de tipos de lenguajes funcionales
- Compilado (no interpretado como Prolog), lo que lo hace significativamente más rápido

================================================================================

APARTADO 2: INSTALACIÓN Y PRIMEROS PASOS (~5 min)
================================================================================

2.1 Requisitos del sistema
---------------------------
- Sistema operativo: Linux, macOS, o Windows (via WSL recomendado)
- Espacio en disco: ~500 MB
- Dependencias: GCC o Clang, make

2.2 Instalación en Linux/Ubuntu
---------------------------------
# Método 1: Desde repositorios (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install mercury-dev

# Método 2: Desde el sitio oficial (versión más reciente)
# Descargar desde: https://www.mercurylang.org/download.html

wget https://dl.mercurylang.org/rotd/mercury-srcdist-ROTD-YYYY-MM-DD.tar.gz
tar xzf mercury-srcdist-*.tar.gz
cd mercury-srcdist-*
./configure
make
sudo make install

2.3 Instalación en macOS
--------------------------
# Con Homebrew
brew install mercury

# Verificar instalación
mmc --version

2.4 Instalación en Windows
----------------------------
# Opción recomendada: WSL2 (Windows Subsystem for Linux)
# 1. Habilitar WSL2
# 2. Instalar Ubuntu desde Microsoft Store
# 3. Seguir instrucciones de Linux dentro de WSL

# Alternativa: usar el instalador binario de Windows desde:
# https://www.mercurylang.org/download.html

2.5 Verificación de la instalación
------------------------------------
mmc --version
# Debería mostrar algo como: Mercury Compiler version 22.01

2.6 Estructura de un archivo Mercury
--------------------------------------
Todo archivo Mercury (.m) tiene la siguiente estructura básica:

:- module nombre_modulo.       % Declaración del módulo
:- interface.                  % Inicio de la interfaz pública

:- import_module io.           % Importar módulos necesarios

:- pred main(io::di, io::uo) is det.  % Declaración del predicado principal

:- implementation.             % Inicio de la implementación

main(!IO) :-
    io.write_string("Hola Mundo\n", !IO).

2.7 Hello World - Primer programa
-----------------------------------
Archivo: hola_mundo.m

:- module hola_mundo.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

main(!IO) :-
    io.write_string("Hola, Mundo desde Mercury!\n", !IO).

Compilar y ejecutar:
  mmc --make hola_mundo
  ./hola_mundo

Salida esperada:
  Hola, Mundo desde Mercury!

Nota sobre !IO:
- !IO es azúcar sintáctica para el par (IO0, IO1)
- Representa el estado de I/O que se pasa de forma explícita
- Garantiza que las operaciones de I/O sean puras y ordenadas

================================================================================

APARTADO 3: TOUR DEL LENGUAJE (~5 min)
================================================================================

3.1 Módulos
------------
Mercury organiza el código en módulos. Cada módulo tiene:
- Una sección :- interface. con las declaraciones públicas
- Una sección :- implementation. con el código privado

:- module matematicas.
:- interface.
:- import_module int.
:- pred suma(int::in, int::in, int::out) is det.
:- pred producto(int::in, int::in, int::out) is det.
:- implementation.

suma(X, Y, Z) :- Z = X + Y.
producto(X, Y, Z) :- Z = X * Y.

3.2 Predicados vs Funciones
-----------------------------
Mercury distingue predicados (relaciones) de funciones:

% Predicado: relación entre argumentos
:- pred duplicar(int::in, int::out) is det.
duplicar(X, Y) :- Y = X * 2.

% Función: retorna un valor directamente
:- func cuadrado(int) = int.
cuadrado(X) = X * X.

% Uso:
% duplicar(5, Y)  → Y = 10  (predicado)
% cuadrado(5)     → 25      (función, usable en expresiones)

3.3 Sistema de Modos
---------------------
Los modos indican la dirección del flujo de datos:

::in    → el argumento es una entrada (instanciado al llamar)
::out   → el argumento es una salida (instanciado al retornar)
::in    → el argumento puede ser de cualquier forma (ground)
::di    → destructive input (para I/O)
::uo    → unique output (para I/O)

Ejemplo:
:- pred sumar(int::in, int::in, int::out) is det.
sumar(A, B, C) :- C = A + B.

3.4 Declaraciones de Determinismo
-----------------------------------
Mercury requiere declarar cuántas soluciones puede tener un predicado:

det      → exactamente 1 solución, nunca falla
semidet  → 0 o 1 soluciones (puede fallar)
nondet   → 0 o más soluciones (no determinístico)
multi    → 1 o más soluciones (siempre tiene al menos una)
failure  → siempre falla
erroneous → siempre lanza excepción

Ejemplos:
:- pred buscar(list(int)::in, int::out) is nondet.  % múltiples soluciones
:- pred es_par(int::in) is semidet.                 % puede fallar
:- pred factorial(int::in, int::out) is det.        % siempre tiene solución

3.5 Sistema de Tipos
---------------------
Mercury tiene tipado estático fuerte con polimorfismo paramétrico:

% Tipos básicos
int, float, char, string, bool

% Tipo lista (polimórfico)
list(T)   % lista de elementos del tipo T

% Tipos definidos por el usuario
:- type forma
    --->    circulo(float)          % radio
    ;       rectangulo(float, float) % ancho, alto
    ;       triangulo(float, float, float). % lados

% Tipo Maybe (opcional)
:- type maybe(T)
    --->    yes(T)
    ;       no.

================================================================================

APARTADO 4: SINTAXIS ESENCIAL (~5 min)
================================================================================

4.1 Términos
-------------
Los términos son los bloques básicos de datos en Mercury:

- Átomos: hola, mundo, true, false
- Números: 42, 3.14, -7
- Variables: X, Y, Resultado (empiezan con mayúscula o _)
- Términos compuestos: f(a, b), par(1, 2)
- Listas: [], [1,2,3], [H|T]

4.2 Unificación
----------------
La unificación es el mecanismo fundamental de "igualdad" en Mercury.
Dos términos se unifican si pueden hacerse idénticos mediante sustitución de variables.

X = 5.           % X queda ligada a 5
par(X, Y) = par(1, 2).  % X=1, Y=2
[H|T] = [1,2,3].        % H=1, T=[2,3]

4.3 Listas
-----------
Las listas son estructuras fundamentales en programación lógica:

% Listas literales
Lista1 = [1, 2, 3, 4, 5].
Lista2 = ["hola", "mundo"].
ListaVacia = [].

% Notación cabeza-cola
[Cabeza | Cola] = [1, 2, 3].  % Cabeza=1, Cola=[2,3]

% Predicados de listas (módulo list)
:- import_module list.

% length/2: longitud de lista
list.length([1,2,3], N).  % N = 3

% append/3: concatenar listas
list.append([1,2], [3,4], L).  % L = [1,2,3,4]

% member/2: pertenencia (nondet)
list.member(X, [1,2,3]).  % X = 1 ; X = 2 ; X = 3

4.4 Pattern Matching
---------------------
Mercury usa pattern matching exhaustivo en la definición de predicados:

% Pattern matching en predicados
:- pred describir_lista(list(int)::in, string::out) is det.
describir_lista([], "La lista está vacía").
describir_lista([_], "La lista tiene un elemento").
describir_lista([_,_|_], "La lista tiene dos o más elementos").

% Pattern matching con if-then-else
:- pred clasificar(int::in, string::out) is det.
clasificar(N, Clase) :-
    ( N > 0 ->
        Clase = "positivo"
    ; N < 0 ->
        Clase = "negativo"
    ;
        Clase = "cero"
    ).

4.5 Recursión
--------------
La recursión es el mecanismo de iteración en programación lógica.

% Suma de elementos de una lista
:- pred suma_lista(list(int)::in, int::out) is det.
suma_lista([], 0).
suma_lista([H|T], Suma) :-
    suma_lista(T, SumaResto),
    Suma = H + SumaResto.

% Longitud de una lista
:- pred longitud(list(T)::in, int::out) is det.
longitud([], 0).
longitud([_|T], N) :-
    longitud(T, N1),
    N = N1 + 1.

% Invertir una lista (con acumulador)
:- pred invertir(list(T)::in, list(T)::out) is det.
invertir(Lista, Invertida) :-
    invertir_aux(Lista, [], Invertida).

:- pred invertir_aux(list(T)::in, list(T)::in, list(T)::out) is det.
invertir_aux([], Acc, Acc).
invertir_aux([H|T], Acc, Resultado) :-
    invertir_aux(T, [H|Acc], Resultado).

================================================================================

APARTADO 5: PARTICULARIDADES DE MERCURY (~4 min)
================================================================================

5.1 Sistema de Tipos Estático con Inferencia
---------------------------------------------
A diferencia de Prolog, Mercury detecta errores de tipo en tiempo de compilación.

% Esto causa error de compilación (tipos incompatibles):
% suma(3.14, "hola", Z)   → ERROR en compilación

% Mercury también infiere tipos en muchos casos:
:- func doblar(int) = int.
doblar(X) = X * 2.   % Mercury infiere que X e Y son int

5.2 I/O Puro con Estado Explícito
-----------------------------------
Mercury maneja I/O de forma completamente pura usando el tipo io.state:

:- pred main(io::di, io::uo) is det.
main(!IO) :-
    io.write_string("¿Cuál es tu nombre? ", !IO),
    io.read_line_as_string(Result, !IO),
    ( Result = ok(Nombre) ->
        io.format("Hola, %s!\n", [s(Nombre)], !IO)
    ;
        io.write_string("Error al leer\n", !IO)
    ).

Explicación del !IO:
- !IO es azúcar para (IO0, IO1, IO2, ...)
- Cada operación recibe el estado anterior y produce uno nuevo
- El compilador verifica el orden correcto de las operaciones
- Esto hace que el I/O sea puro y determinista

5.3 Diferencias clave con Prolog
----------------------------------

| Característica      | Prolog           | Mercury                    |
|---------------------|------------------|----------------------------|
| Tipos               | Dinámico         | Estático con inferencia    |
| Modos               | Implícitos       | Declarados explícitamente  |
| Determinismo        | Implícito        | Declarado explícitamente   |
| I/O                 | Predicados impuros| Estado explícito (puro)   |
| Corte (!)           | Soportado        | No existe (se usa if-then) |
| Rendimiento         | Interpretado     | Compilado a C nativo       |
| assert/retract      | Soportado        | No existe (inmutable)      |
| Módulos             | Limitado         | Sistema completo           |

5.4 Variables únicas y !
--------------------------
Mercury introduce el operador ! para variables de estado:

% Sin azúcar sintáctica:
main(IO0, IO) :-
    io.write_string("Hola\n", IO0, IO1),
    io.write_string("Mundo\n", IO1, IO).

% Con azúcar !IO (equivalente):
main(!IO) :-
    io.write_string("Hola\n", !IO),
    io.write_string("Mundo\n", !IO).

5.5 Compilación y rendimiento
-------------------------------
Mercury compila a C y luego a código nativo:

Fuente .m → Compilador Mercury → Código C → GCC → Ejecutable nativo

Esto resulta en rendimiento comparable a C para muchas tareas, muy superior a Prolog interpretado.

================================================================================

APARTADO 6: EJEMPLOS EN JUPYTER NOTEBOOK (~10 min)
================================================================================

---------- EJEMPLOS BÁSICOS ----------

EJEMPLO B1: Factorial
----------------------
% Archivo: factorial.m
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
    io.format("factorial(5) = %d\n", [i(factorial(5))], !IO),
    io.format("factorial(10) = %d\n", [i(factorial(10))], !IO).

% Salida:
% factorial(5) = 120
% factorial(10) = 3628800

EJEMPLO B2: Fibonacci
----------------------
:- module fibonacci.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int.

:- func fib(int) = int.
fib(0) = 0.
fib(1) = 1.
fib(N) = fib(N-1) + fib(N-2) :- N > 1.

main(!IO) :-
    io.write_string("Secuencia de Fibonacci:\n", !IO),
    print_fibs(0, 10, !IO).

:- pred print_fibs(int::in, int::in, io::di, io::uo) is det.
print_fibs(I, Max, !IO) :-
    ( I < Max ->
        io.format("fib(%d) = %d\n", [i(I), i(fib(I))], !IO),
        print_fibs(I + 1, Max, !IO)
    ;
        true
    ).

% Salida:
% fib(0) = 0
% fib(1) = 1
% fib(2) = 1
% fib(3) = 2
% ...

EJEMPLO B3: Operaciones básicas con números
--------------------------------------------
:- module operaciones.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, float.

main(!IO) :-
    % Operaciones enteras
    A = 10, B = 3,
    io.format("%d + %d = %d\n", [i(A), i(B), i(A + B)], !IO),
    io.format("%d - %d = %d\n", [i(A), i(B), i(A - B)], !IO),
    io.format("%d * %d = %d\n", [i(A), i(B), i(A * B)], !IO),
    io.format("%d div %d = %d\n", [i(A), i(B), i(A div B)], !IO),
    io.format("%d mod %d = %d\n", [i(A), i(B), i(A mod B)], !IO).

---------- EJEMPLOS INTERMEDIOS ----------

EJEMPLO I1: Manipulación de listas
------------------------------------
:- module listas.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list, int.

% Suma de todos los elementos de una lista
:- func suma_lista(list(int)) = int.
suma_lista([]) = 0.
suma_lista([H|T]) = H + suma_lista(T).

% Máximo de una lista (semidet: puede fallar si la lista está vacía)
:- pred maximo(list(int)::in, int::out) is semidet.
maximo([X], X).
maximo([H|T], Max) :-
    maximo(T, MaxT),
    ( H > MaxT -> Max = H ; Max = MaxT ).

% Filtrar elementos que cumplen una condición
:- pred filtrar_pares(list(int)::in, list(int)::out) is det.
filtrar_pares([], []).
filtrar_pares([H|T], Resultado) :-
    filtrar_pares(T, RestoFiltrado),
    ( H mod 2 = 0 ->
        Resultado = [H | RestoFiltrado]
    ;
        Resultado = RestoFiltrado
    ).

% Aplicar función a todos los elementos (map)
:- pred map_doblar(list(int)::in, list(int)::out) is det.
map_doblar([], []).
map_doblar([H|T], [H*2 | Resto]) :-
    map_doblar(T, Resto).

main(!IO) :-
    Lista = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3],
    io.format("Lista original: %s\n",
        [s(string(Lista))], !IO),
    io.format("Suma: %d\n",
        [i(suma_lista(Lista))], !IO),
    ( maximo(Lista, Max) ->
        io.format("Máximo: %d\n", [i(Max)], !IO)
    ;
        io.write_string("Lista vacía\n", !IO)
    ),
    filtrar_pares(Lista, Pares),
    io.format("Pares: %s\n", [s(string(Pares))], !IO).

EJEMPLO I2: Búsqueda con backtracking
---------------------------------------
:- module busqueda.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list, int, solutions.

% Pertenencia a una lista (nondet - múltiples soluciones)
:- pred pertenece(T::out, list(T)::in) is nondet.
pertenece(X, [X|_]).
pertenece(X, [_|T]) :- pertenece(X, T).

% Encontrar pares que sumen N
:- pred par_suma(int::in, list(int)::in, int::out, int::out) is nondet.
par_suma(N, Lista, X, Y) :-
    pertenece(X, Lista),
    pertenece(Y, Lista),
    X < Y,
    X + Y = N.

main(!IO) :-
    Lista = [1, 2, 3, 4, 5, 6, 7, 8],
    Objetivo = 9,
    io.format("Pares que suman %d en %s:\n",
        [i(Objetivo), s(string(Lista))], !IO),
    % solutions/2 recolecta todas las soluciones de un predicado nondet
    solutions(
        (pred(par(X, Y)::out) is nondet :-
            par_suma(Objetivo, Lista, X, Y)),
        Soluciones),
    list.foldl(
        (pred(par(X, Y)::in, !.IO::di, !:IO::uo) is det :-
            io.format("  %d + %d = %d\n",
                [i(X), i(Y), i(Objetivo)], !IO)),
        Soluciones, !IO).

:- type par ---> par(int, int).

EJEMPLO I3: Tipos algebraicos y pattern matching
--------------------------------------------------
:- module formas.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module float, math.

:- type forma
    --->    circulo(float)
    ;       rectangulo(float, float)
    ;       triangulo(float, float, float).

:- func area(forma) = float.
area(circulo(R)) = math.pi * R * R.
area(rectangulo(A, B)) = A * B.
area(triangulo(A, B, C)) = Area :-
    % Fórmula de Herón
    S = (A + B + C) / 2.0,
    Area = math.sqrt(S * (S-A) * (S-B) * (S-C)).

:- func perimetro(forma) = float.
perimetro(circulo(R)) = 2.0 * math.pi * R.
perimetro(rectangulo(A, B)) = 2.0 * (A + B).
perimetro(triangulo(A, B, C)) = A + B + C.

:- pred describir(forma::in, io::di, io::uo) is det.
describir(Forma, !IO) :-
    io.format("Área: %.2f | Perímetro: %.2f\n",
        [f(area(Forma)), f(perimetro(Forma))], !IO).

main(!IO) :-
    Formas = [
        circulo(5.0),
        rectangulo(3.0, 4.0),
        triangulo(3.0, 4.0, 5.0)
    ],
    list.foldl(describir, Formas, !IO).

---------- EJEMPLOS AVANZADOS ----------

EJEMPLO A1: Módulo propio con interfaz completa
------------------------------------------------
% Archivo: pila.m - Implementación de una Pila (Stack)

:- module pila.
:- interface.
:- import_module list.

% Tipo abstracto - los usuarios no ven la implementación
:- type pila(T).

% Operaciones de la pila
:- func pila_vacia = pila(T).
:- pred push(T::in, pila(T)::in, pila(T)::out) is det.
:- pred pop(pila(T)::in, T::out, pila(T)::out) is semidet.
:- pred peek(pila(T)::in, T::out) is semidet.
:- pred es_vacia(pila(T)::in) is semidet.
:- func tamanio(pila(T)) = int.

:- implementation.
:- import_module int.

% La pila es internamente una lista
:- type pila(T) ---> pila(list(T)).

pila_vacia = pila([]).

push(Elem, pila(Lista), pila([Elem|Lista])).

pop(pila([H|T]), H, pila(T)).

peek(pila([H|_]), H).

es_vacia(pila([])).

tamanio(pila(Lista)) = list.length(Lista).

% Archivo: uso_pila.m - Uso del módulo pila
:- module uso_pila.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module pila, int.

main(!IO) :-
    P0 = pila_vacia,
    push(10, P0, P1),
    push(20, P1, P2),
    push(30, P2, P3),
    io.format("Tamaño de la pila: %d\n", [i(tamanio(P3))], !IO),
    ( peek(P3, Tope) ->
        io.format("Tope: %d\n", [i(Tope)], !IO)
    ;
        true
    ),
    ( pop(P3, Elem, P4) ->
        io.format("Pop: %d\n", [i(Elem)], !IO),
        io.format("Nuevo tamaño: %d\n", [i(tamanio(P4))], !IO)
    ;
        true
    ).

EJEMPLO A2: I/O compleja con manejo de errores
-----------------------------------------------
:- module calculadora_io.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, string, char, list.

:- type operacion
    --->    suma
    ;       resta
    ;       multiplicacion
    ;       division.

:- pred parse_operacion(string::in, operacion::out) is semidet.
parse_operacion("+", suma).
parse_operacion("-", resta).
parse_operacion("*", multiplicacion).
parse_operacion("/", division).

:- pred ejecutar(int::in, operacion::in, int::in, int::out) is semidet.
ejecutar(A, suma, B, A + B).
ejecutar(A, resta, B, A - B).
ejecutar(A, multiplicacion, B, A * B).
ejecutar(A, division, B, A div B) :- B \= 0.

:- pred leer_entero(string::in, int::out, io::di, io::uo) is det.
leer_entero(Prompt, N, !IO) :-
    io.write_string(Prompt, !IO),
    io.read_line_as_string(Result, !IO),
    ( Result = ok(Linea),
      string.to_int(string.strip(Linea), N0)
    ->
        N = N0
    ;
        io.write_string("Número inválido, usando 0\n", !IO),
        N = 0
    ).

main(!IO) :-
    io.write_string("=== Calculadora Mercury ===\n", !IO),
    leer_entero("Primer número: ", A, !IO),
    io.write_string("Operación (+, -, *, /): ", !IO),
    io.read_line_as_string(OpResult, !IO),
    leer_entero("Segundo número: ", B, !IO),
    ( OpResult = ok(OpStr),
      parse_operacion(string.strip(OpStr), Op),
      ejecutar(A, Op, B, Resultado)
    ->
        io.format("Resultado: %d\n", [i(Resultado)], !IO)
    ;
        io.write_string("Operación inválida o división por cero\n", !IO)
    ).

EJEMPLO A3: Programación lógica pura - Solucionador de N-Reinas
-----------------------------------------------------------------
:- module n_reinas.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, list, solutions.

% Verificar si una posición es segura respecto a las reinas ya colocadas
:- pred segura(int::in, int::in, list(int)::in) is semidet.
segura(_, _, []).
segura(Fila, Col, [ReinaCol | Resto]) :-
    ReinaFila = list.length(Resto) + 1,
    Col \= ReinaCol,
    abs(Col - ReinaCol) \= abs(Fila - ReinaFila),
    segura(Fila, Col, Resto).

% Colocar N reinas en el tablero
:- pred reinas(int::in, int::in, list(int)::in, list(int)::out) is nondet.
reinas(N, Fila, Acc, Solucion) :-
    ( Fila > N ->
        Solucion = Acc
    ;
        between(1, N, Col),
        segura(Fila, Col, Acc),
        reinas(N, Fila + 1, [Col | Acc], Solucion)
    ).

:- pred between(int::in, int::in, int::out) is nondet.
between(Low, High, X) :-
    Low =< High,
    ( X = Low ; between(Low + 1, High, X) ).

:- pred imprimir_tablero(int::in, list(int)::in, io::di, io::uo) is det.
imprimir_tablero(N, Reinas, !IO) :-
    Filas = list.reverse(Reinas),
    list.foldl2(
        (pred(Col::in, Fila::in, NuevaFila::out, !.IO::di, !:IO::uo) is det :-
            NuevaFila = Fila + 1,
            imprime_fila(N, Col, !IO)),
        Filas, 1, _, !IO),
    io.write_string("\n", !IO).

:- pred imprime_fila(int::in, int::in, io::di, io::uo) is det.
imprime_fila(N, ReenaCol, !IO) :-
    ( N >= 1 ->
        ( ReenaCol = N ->
            io.write_string("Q ", !IO)
        ;
            io.write_string(". ", !IO)
        ),
        imprime_fila(N - 1, ReenaCol, !IO)
    ;
        io.write_string("\n", !IO)
    ).

main(!IO) :-
    N = 8,
    io.format("Soluciones al problema de las %d-reinas:\n", [i(N)], !IO),
    solutions(reinas(N, 1, [], ), Soluciones),
    io.format("Total de soluciones: %d\n\n", [i(list.length(Soluciones))], !IO),
    % Mostrar primera solución
    ( Soluciones = [Primera | _] ->
        io.write_string("Primera solución:\n", !IO),
        imprimir_tablero(N, Primera, !IO)
    ;
        io.write_string("Sin soluciones\n", !IO)
    ).

================================================================================

APARTADO 7: CIERRE Y RECURSOS (~3 min)
================================================================================

7.1 Ventajas de Mercury
------------------------
1. Seguridad de tipos: los errores de tipo se detectan en compilación, no en ejecución
2. Rendimiento: compilación a C nativo → velocidad comparable a C
3. Pureza: sin efectos secundarios implícitos → código más predecible
4. Declaratividad: el código describe QUÉ, no CÓMO
5. Verificación formal: el determinismo declarado es verificado por el compilador
6. Módulos robustos: separación clara de interfaz e implementación
7. Backtracking controlado: no hay corte (!) que ensucie la semántica

7.2 Limitaciones de Mercury
-----------------------------
1. Curva de aprendizaje pronunciada: modos, determinismo y tipos son obligatorios
2. Comunidad pequeña: menos recursos y bibliotecas comparado con Prolog
3. Verbosidad: más declaraciones requeridas que Prolog
4. Sin assert/retract: no se puede modificar el programa en tiempo de ejecución
5. Herramientas limitadas: pocos IDEs con soporte nativo
6. Documentación escasa: principalmente la referencia oficial y papers académicos

7.3 Aplicaciones reales
-------------------------
Mercury se ha usado en:
- Compiladores y herramientas de análisis estático
- Sistemas de verificación formal
- Inteligencia artificial y sistemas expertos
- Procesamiento de lenguaje natural
- Software de alta integridad (sistemas críticos)
- El propio compilador de Mercury está escrito en Mercury

Dominios donde la programación lógica brilla:
- Parsing y análisis de lenguajes
- Satisfacción de restricciones (CSP)
- Bases de datos deductivas
- Razonamiento automatizado
- Planificación y scheduling

7.4 Comparación con otros lenguajes del paradigma
---------------------------------------------------
| Lenguaje  | Año  | Tipado   | Compilado | Determinismo | Uso principal      |
|-----------|------|----------|-----------|--------------|-------------------|
| Prolog    | 1972 | Dinámico | Interp.   | Implícito    | IA, investigación |
| Mercury   | 1995 | Estático | Nativo    | Declarado    | Software producción|
| Datalog   | 1978 | Dinámico | -         | -            | Bases de datos    |
| Oz/Mozart | 1991 | Dinámico | Interp.   | Implícito    | Multi-paradigma   |

7.5 Referencias y Bibliografía
--------------------------------
[1] Henderson, F., Conway, T., Somogyi, Z. (1996). "Mercury: An Efficient Purely Declarative Logic Programming Language". Proceedings of the Australian Computer Science Conference.

[2] Sitio oficial de Mercury: https://www.mercurylang.org/

[3] Documentación de referencia: https://www.mercurylang.org/documentation/reference_manual.html

[4] Tutorial oficial: https://www.mercurylang.org/documentation/mercury_users_guide.html

[5] Repositorio GitHub: https://github.com/Mercury-Language/mercury

[6] Somogyi, Z., Henderson, F., Conway, T. (1996). "The Execution Algorithm of Mercury, an Efficient Purely Declarative Logic Programming Language". Journal of Logic Programming.

[7] Página del paradigma (referencia del curso):
    http://ferestrepoca.github.io/paradigmas-de-programacion/

[8] Kernels de Jupyter para diferentes lenguajes:
    https://github.com/ipython/ipython/wiki/IPython-kernels-for-other-languages

================================================================================
FIN DEL DOCUMENTO
================================================================================
Generado para: Lenguajes de Programación - Universidad Nacional de Colombia
Paradigma: Programación Lógica | Lenguaje: Mercury

