:- module dcg_parser.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, char, string, int.

% Gramática: oracion --> frase_nominal, frase_verbal
%             frase_nominal --> sustantivo
%             frase_verbal  --> verbo | verbo, frase_nominal

:- pred sustantivo(list(string)::in, list(string)::out) is semidet.
sustantivo --> ["mercury"].
sustantivo --> ["lenguaje"].
sustantivo --> ["programador"].

:- pred verbo(list(string)::in, list(string)::out) is semidet.
verbo --> ["es"].
verbo --> ["usa"].

:- pred frase_nominal(list(string)::in, list(string)::out) is semidet.
frase_nominal --> sustantivo.

:- pred frase_verbal(list(string)::in, list(string)::out) is semidet.
frase_verbal --> verbo.
frase_verbal --> verbo, frase_nominal.

:- pred oracion(list(string)::in, list(string)::out) is semidet.
oracion --> frase_nominal, frase_verbal.

:- pred probar(string::in, list(string)::in, io::di, io::uo) is det.
probar(Desc, Tokens, !IO) :-
    ( oracion(Tokens, []) ->
        io.format("VALIDA  | %s\n", [s(Desc)], !IO)
    ;
        io.format("INVALIDA| %s\n", [s(Desc)], !IO)
    ).

main(!IO) :-
    probar("mercury es",           ["mercury", "es"],           !IO),
    probar("programador usa mercury",["programador","usa","mercury"], !IO),
    probar("es mercury",           ["es", "mercury"],           !IO).
