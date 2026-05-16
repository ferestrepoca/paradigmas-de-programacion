% !IO es azucar sintactico para IO0::di, IO::uo
% Cada llamada consume el estado anterior y produce uno nuevo.
% El compilador garantiza que no se use el mismo estado dos veces.

main(!IO) :-
    io.write_string("Linea 1\n", !IO),   % consume IO0, produce IO1
    io.write_string("Linea 2\n", !IO),   % consume IO1, produce IO2
    io.write_string("Linea 3\n", !IO).   % consume IO2, produce IO3

% Nota: !IO expandido es:
%   main(IO0, IO3) :-
%       io.write_string("Linea 1\n", IO0, IO1),
%       io.write_string("Linea 2\n", IO1, IO2),
%       io.write_string("Linea 3\n", IO2, IO3).
