hanoi(0,_,_,_).
hanoi(N,Origen,Auxiliar,Destino):- N1 is N-1,
    hanoi(N1,Origen,Destino,Auxiliar),
    def_pasos(N,Origen,Destino),
    hanoi(N1,Auxiliar,Origen,Destino),
    writeln('Fiiiin').

def_pasos(N,Origen,Destino):-
write(' ficha '),
write(N),    
write(' desde '),
write(Origen),
write(' hasta '),
write(Destino),
writeln('\n'). 