:- module rot13.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, list, string.

main(!IO) :-
    io.read_char(Result, !IO),
    (
        Result = ok(Char),
        io.write_char(rot13(Char), !IO),
        main(!IO)
    ;
        Result = eof
    ;
        Result = error(ErrorCode),
        io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
    ).

:- func rot13(char) = char.

rot13(Char) = ( if          Char = 'a'  then 'n'
                else if     Char = 'b'  then 'o'
                else if     Char = 'c'  then 'p'
                else if     Char = 'd'  then 'q'
                else if     Char = 'e'  then 'r'
                else if     Char = 'f'  then 's'
                else if     Char = 'g'  then 't'
                else if     Char = 'h'  then 'u'
                else if     Char = 'i'  then 'v'
                else if     Char = 'j'  then 'w'
                else if     Char = 'k'  then 'x'
                else if     Char = 'l'  then 'y'
                else if     Char = 'm'  then 'z'
                else if     Char = 'n'  then 'a'
                else if     Char = 'o'  then 'b'
                else if     Char = 'p'  then 'c'
                else if     Char = 'q'  then 'f'
                else if     Char = 'r'  then 'e'
                else if     Char = 's'  then 'f'
                else if     Char = 't'  then 'g'
                else if     Char = 'u'  then 'h'
                else if     Char = 'v'  then 'i'
                else if     Char = 'w'  then 'j'
                else if     Char = 'x'  then 'k'
                else if     Char = 'y'  then 'l'
                else if     Char = 'z'  then 'm'
                else        Char
              ).  