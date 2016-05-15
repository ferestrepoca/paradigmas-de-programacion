%juan y pedro son hermanos
hermano("juan", "pedro").
hermano("pedro", "jose").
hijo("juancito", "juan").
hijo("ana", "juan").
hijo("natalia", "pedro").
hijo("andres", "jose").
tio(X,Y):- (hermano(Z,X);hermano(X,Z)),hijo(Y,Z).
