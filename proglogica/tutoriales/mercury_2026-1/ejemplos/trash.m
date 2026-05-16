:- module trash.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, int.

% Tipo personalizado que encapsula un entero.
:- type my_data ---> data(int).

% Crea una lista de N elementos my_data (de N hacia 1).
:- func create_data_list(int) = list(my_data).
create_data_list(N) =
    ( if N =< 0 then
        []
    else
        [data(N) | create_data_list(N - 1)]
    ).

% Convierte la lista de my_data a una lista de int.
:- func process_data_list(list(my_data)) = list(int).
process_data_list([]) = [].
process_data_list([data(X) | Xs]) = [X | process_data_list(Xs)].

main(!IO) :-
    % Crear lista con 10 elementos.
    DataList = create_data_list(10),
    % Procesar: extraer los enteros.
    IntList  = process_data_list(DataList),
    % Imprimir.
    io.write_list(IntList, ", ", io.write_int, !IO),
    io.nl(!IO),
    % DataList e IntList ya no son referenciadas aquí.
    % El recolector de basura (Boehm GC) libera su memoria automáticamente.
    io.write_string(
        "Memoria gestionada automáticamente por el recolector de basura.\n",
        !IO).
