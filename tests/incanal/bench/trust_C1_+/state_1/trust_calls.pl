:- module(_,[main/2, main2/2, main3/1],[assertions]).

main(X,Y) :-
    p(X,Y).

%:- pred p(X,Y) : ground(X).
p(X,Y) :-
    X = Y.

main2(X,Y) :-
    p2(X,Y),
    p2(X,Y).

%:- pred p2(X,Y) : ground(X).
p2(X,Y) :-
    X = Y.


main3(Y) :-
    p3(X,Y),
    p3(X,Y).

%:- pred p3(X,Y) : ground(X).
p3(X,Y) :-
    X = Y.
