:- module(_,[main/1],[assertions]).

main(X) :-
    X = a,
    p(X,Y).

:- pred p(X,Y) : var(X) => atm(Y).
p(X,Y) :-
    X = Y.
