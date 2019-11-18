:- module(_,[main/2],[assertions]).

main(X,Y) :-
    p(X,Y),
    X = a,
    p(X,Y).

%:- pred p(X,Y) : ground(X).
p(X,Y) :-
    X = Y.
