:- module(read_string, [read_string/3], []).

:- use_module(engine(io_basic)).
:- use_module(my_ttyout, [ttyput/1, ttynl/0]).

%   read_string(Chars, Quote, NextCh)
%   reads the body of a string delimited by Quote characters.
%   The result is a list of ASCII codes.  There are two complications.
%   If we hit the end of the file inside the string this predicate FAILS.
%   It does not return any special structure.  That is the only reason
%   it can ever fail.  The other complication is that when we find a Quote
%   we have to look ahead one character in case it is doubled.  Note that
%   if we find an end-of-file after the quote we *don't* fail, we return
%   a normal string and the end of file character is returned as NextCh.
%   If we were going to accept C-like escape characters, as I think we
%   should, this would need changing (as would the code for 0'x).  But
%   the purpose of this module is not to present my ideal syntax but to
%   present something which will read present-day Prolog programs.

read_string(Chars, Quote, NextCh) :-
	get_code(Ch),
	read_string(Ch, Chars, Quote, NextCh).


read_string(-1, _N, Quote, -1) :-
	display('! end of file in '), ttyput(Quote),
	display(token), ttyput(Quote), ttynl,
	!, fail.
read_string(Quote, Chars, Quote, NextCh) :- !,
	get_code(Ch),				% closing or doubled quote
	more_string(Ch, Quote, Chars, NextCh).
read_string(Char, [Char|Chars], Quote, NextCh) :-
	read_string(Chars, Quote, NextCh).	% ordinary character

more_string(Quote, Quote, [Quote|Chars], NextCh) :- !,
	read_string(Chars, Quote, NextCh).	% doubled quote
more_string(NextCh, _, [], NextCh).		% end

