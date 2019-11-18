:- module(_, [read_integer/3], []).

:- use_module(engine(io_basic)).

%   read_integer is complicated by having to understand radix notation.
%   There are three forms of integer:
%       0 ' <any character>     - the ASCII code for that character
%       <digit> ' <digits>      - the digits, read in that base
%       <digits>                - the digits, read in base 10.
%   Note that radix 16 is not understood, because 16 is two digits,
%   and that all the decimal digits are accepted in each base (this
%   is also true of C).  So 2'89 = 25.  I can't say I care for this,
%   but it does no great harm, and that's what Dec-10 Prolog does.
%   The X =\= -1 tests are to make sure we don't miss an end of file
%   character.  The tokeniser really should be in C, not least to
%   make handling end of file characters bearable.  If we hit an end
%   of file inside an integer, read_integer will fail.

read_integer(BaseChar, IntVal, NextCh) :-
    Base is BaseChar - 48,
    get_code(Ch),
    Ch =\= -1,
    (   Ch =\= 39, read_digits(Ch, Base, 10, IntVal, NextCh)
    ;   Base >= 1, read_digits(0, Base, IntVal, NextCh)
    ;   get_code(IntVal), IntVal =\= -1, get_code(NextCh)
    ),  !.

read_digits(SoFar, Base, Value, NextCh) :-
    get_code(Ch),
    Ch =\= -1,
    read_digits(Ch, SoFar, Base, Value, NextCh).

read_digits(Digit, SoFar, Base, Value, NextCh) :-
    Digit >= 48, Digit =< 57,
    !,
    Next is SoFar*Base-48+Digit,
    read_digits(Next, Base, Value, NextCh).
read_digits(LastCh, Value, _, Value, LastCh).
