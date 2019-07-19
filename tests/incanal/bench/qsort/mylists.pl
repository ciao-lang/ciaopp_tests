:- module(mylists,[append/3,length/2],[assertions]).


%:- entry length(A,B) : ( term_typing:var(A), term_typing:ground([B]) ).
%:- entry length(A,B) : ( term_typing:var(B), term_typing:ground([A]) ).

length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

llength([], I, I).
llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

dlength([], I, I) :- !.
dlength([_|L], I0, I) :- I0<I, I1 is I0+1, dlength(L, I1, I).

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).
