:- module(_,[main/2,main2/2,main3/2],[assertions]).

main(X,Y) :-
        X = a,
        p(X,Y).

:- pred p(X,Y) : term(X).
:- pred p(X,Y) : atm(X) => int(Y).
p(X,_) :-
        X = a.

main2(X,Y) :-
        p2(X,Y).

:- pred p2(X,Y) => int(Y).
p2(X,_) :-
        X = a.

main3(X,Y) :-
	p3(X,Y).

:- pred p3(X,Y) => int(Y).
p3(X,_).
p3(X,_) :-
        X = a.