:- module(_,[main/1],[assertions]).

main(Y) :-
        X = a,
        p(X,Y).

:- pred p(X,Y) : ground(X).
p(X,Y) :-
        X = Y.