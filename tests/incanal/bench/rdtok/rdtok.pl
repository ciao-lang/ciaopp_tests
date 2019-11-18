:- module(rdtok, [read_tokens/2], []).

:- use_module(engine(io_basic)).

:- use_module(my_ttyout, [ttynl/0]).
:- use_module(read_string, [read_string/3]).
:- use_module(read_integer, [read_integer/3]).
:- use_module(my_lists, [mymember/2, append/3]).

read_tokens(TokenList, Dictionary) :-
    read_tokens(32, Dict, ListOfTokens),
    append(Dict, [], Dict), !,      %  fill in the "hole" at the end
    Dictionary = Dict,              %  unify explicitly so we'll read and
    TokenList = ListOfTokens.       %  then check even with filled in arguments
read_tokens([atom(end_of_file)], []).   %  End Of File is all that can go wrong


read_tokens(-1, _X, _Y) :- !,           %  -1 is the end-of-file character
    fail.                           %  in every standard Prolog
read_tokens(Ch, Dict, Tokens) :-
    Ch =< 32,                       %  ignore layout.  CR, LF, and the
    !,                              %  ASCII newline character (10)
    get_code(NextCh),                       %  are all skipped here.
    read_tokens(NextCh, Dict, Tokens).
read_tokens(37, Dict, Tokens) :- !,     %  %comment
    repeat,                         %  skip characters to a line
        get_code(Ch),                       %  terminator (should we be
        ( Ch = 10 ; Ch = -1 ),      %  more thorough, e.g. ^L?)
    !,                              %  stop when we find one
    Ch =\= -1,                      %  fail on EOF
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(47, Dict, Tokens) :- !,     %  /*comment?
    get_code(NextCh),
    read_solidus(NextCh, Dict, Tokens).
read_tokens(33, Dict, [atom(!)|Tokens]) :- !,   %  This is a special case so
    get_code(NextCh),                       %  that !. reads as two tokens.
    read_after_atom(NextCh, Dict, Tokens).  %  It could be cleverer.
read_tokens(40, Dict, [' ('|Tokens]) :- !,      %  NB!!!
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(41, Dict, [')'|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(44, Dict, [','|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(59, Dict, [atom((;))|Tokens]) :- !, % ; is nearly a punctuation
    get_code(NextCh),                               %  mark but not quite (e.g.
    read_tokens(NextCh, Dict, Tokens).      %  you can :-op declare it).
read_tokens(91, Dict, ['['|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(93, Dict, [']'|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(123, Dict, ['{'|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(124, Dict, ['|'|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(125, Dict, ['}'|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(46, Dict, Tokens) :- !,             % full stop
    get_code(NextCh),                               % or possibly .=. &c
    read_fullstop(NextCh, Dict, Tokens).
read_tokens(34, Dict, [string(S)|Tokens]) :- !, % "string"
    read_string(S, 34, NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(39, Dict, [atom(A)|Tokens]) :- !,   % 'atom'
    read_string(S, 39, NextCh),
    name(A, S),             % BUG: '0' = 0 unlike Dec-10 Prolog
    read_after_atom(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [var(Var,Name)|Tokens]) :-
    (  Ch = 95 ; Ch >= 65, Ch =< 90  ),     % _ or A..Z
    !,                                      % have to watch out for "_"
    read_name(Ch, S, NextCh),
    (  S = "_", Name = '_'                  % anonymous variable
    ;  name(Name, S),                       % construct name
       read_lookup(Dict, Name=Var)          % lookup/enter in dictionary
    ), !,
    read_tokens(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [integer(I)|Tokens]) :-
    Ch >= 48, Ch =< 57,     
    !,
    read_integer(Ch, I, NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [atom(A)|Tokens]) :-
    Ch >= 97, Ch =< 122,                    % a..z
    !,                                      % no corresponding _ problem
    read_name(Ch, S, NextCh),
    name(A, S),
    read_after_atom(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [atom(A)|Tokens]) :-      % THIS MUST BE THE LAST CLAUSE
    get_code(AnotherCh),
    read_symbol(AnotherCh, Chars, NextCh),  % might read 0 chars
    name(A, [Ch|Chars]),                    % so might be [Ch]
    read_after_atom(NextCh, Dict, Tokens).

%   The only difference between read_after_atom(Ch, Dict, Tokens) and
%   read_tokens/3 is what they do when Ch is "(".  read_after_atom
%   finds the token to be '(', while read_tokens finds the token to be
%   ' ('.  This is how the parser can tell whether <atom> <paren> must
%   be an operator application or an ordinary function symbol application.
%   See the library file READ.PL for details.

read_after_atom(40, Dict, ['('|Tokens]) :- !,
    get_code(NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_after_atom(Ch, Dict, Tokens) :-
    read_tokens(Ch, Dict, Tokens).

%   read_solidus(Ch, Dict, Tokens)
%   checks to see whether /Ch is a /* comment or a symbol.  If the
%   former, it skips the comment.  If the latter it just calls read_symbol.
%   We have to take great care with /* comments to handle end of file
%   inside a comment, which is why read_solidus/2 passes back an end of
%   file character or a (forged) blank that we can give to read_tokens.

read_solidus(42, Dict, Tokens) :- !,
    get_code(Ch),
    read_solidus(Ch, NextCh),
    read_tokens(NextCh, Dict, Tokens).
read_solidus(Ch, Dict, [atom(A)|Tokens]) :-
    read_symbol(Ch, Chars, NextCh),         % might read 0 chars
    name(A, [47|Chars]),
    read_tokens(NextCh, Dict, Tokens).

read_solidus(-1, -1) :- !,
    display('! end of file in /*comment'), ttynl.
read_solidus(42, LastCh) :-
    get_code(NextCh),
    NextCh =\= 47, !,       %  might be ^Z or * though
    read_solidus(NextCh, LastCh).
read_solidus(42, 32) :- !.      %  the / was skipped in the previous clause
read_solidus(_N, LastCh) :-
    get_code(NextCh),
    read_solidus(NextCh, LastCh).


%   read_name(Char, String, LastCh)
%   reads a sequence of letters, digits, and underscores, and returns
%   them as String.  The first character which cannot join this sequence
%   is returned as LastCh.

read_name(Char, [Char|Chars], LastCh) :-
    ( Char >= 97, Char =< 122       % a..z
    ; Char >= 65, Char =< 90        % A..Z
    ; Char >= 48, Char =< 57        % 0..9
    ; Char = 95                     % _
    ), !,
    get_code(NextCh),
    read_name(NextCh, Chars, LastCh).
read_name(LastCh, [], LastCh).


%   read_symbol(Ch, String, NextCh)
%   reads the other kind of atom which needs no quoting: one which is
%   a string of "symbol" characters.  Note that it may accept 0
%   characters, this happens when called from read_fullstop.

read_symbol(Char, [Char|Chars], LastCh) :-
    mymember(Char, "#$&*+-./:<=>?@\\^`~"),
    get_code(NextCh),
    read_symbol(NextCh, Chars, LastCh).
read_symbol(LastCh, [], LastCh).


%   read_fullstop(Char, Dict, Tokens)
%   looks at the next character after a full stop.  There are
%   three cases:
%       (a) the next character is an end of file.  We treat this
%           as an unexpected end of file.  The reason for this is
%           that we HAVE to handle end of file characters in this
%           module or they are gone forever; if we failed to check
%           for end of file here and just accepted .<EOF> like .<NL>
%           the caller would have no way of detecting an end of file
%           and the next call would abort.
%       (b) the next character is a layout character.  This is a
%           clause terminator.
%       (c) the next character is anything else.  This is just an
%           ordinary symbol and we call read_symbol to process it.

read_fullstop(-1, _X, _Y) :- !,
    display('! end of file just after full stop'), ttynl,
    fail.
read_fullstop(Ch, _N, []) :-
    Ch =< 32, !.            % END OF CLAUSE
read_fullstop(Ch, Dict, [atom(A)|Tokens]) :-
    read_symbol(Ch, S, NextCh),
    name(A, [46|S]),
    read_tokens(NextCh, Dict, Tokens).


%   read_lookup is identical to member except for argument order and
%   mode declaration..

read_lookup([X|_], X) :- !.
read_lookup([_M|T], X) :-
    read_lookup(T, X). 
 
