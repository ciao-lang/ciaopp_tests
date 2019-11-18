:- module(_,[main/1,main2/2],[assertions]).

main(X) :-
    X = a,
    p(X).

%:- pred p(X) : var(X).
p(X).


main2(X,Y) :-
    X = a,
    p2(X,Y).

%:- pred p2(X,Y) : var(X).
p2(X,Y) :-
    X = Y.
