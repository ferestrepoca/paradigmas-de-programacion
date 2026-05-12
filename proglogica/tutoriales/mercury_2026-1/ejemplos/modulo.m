:- module mi_modulo.          % 1. Nombre del módulo (coincide con el archivo .m)

:- interface.                  % 2. INTERFAZ: lo que es público
:- import_module io.           %    Importar módulo de E/S
:- pred main(io::di, io::uo) is det.   % Declarar predicado principal

:- implementation.             % 3. IMPLEMENTACIÓN: código privado
main(!IO) :-
    io.write_string("Hola desde Mercury!\n", !IO).
