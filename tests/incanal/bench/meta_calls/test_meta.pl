:- module(_, [test/2], []).

test(A, L) :-
    catch(handler(A), _, true).

handler(a).
handler(_).
