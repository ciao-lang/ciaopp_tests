:- module(_,[p/1,q/1,k/1],[assertions]).

p(X) :-
    rp(X).

:- trust calls rp(X) : int(X).
:- trust calls rp(X) : var(X).
rp(X).


q(X) :-
    rq(X).

%:- trust calls rq(X) : int(X).
%:- trust calls rq(X) : var(X).
rq(X) :-
    X = a.


k(X) :-
    rk(a).

:- trust calls rk(X) : var(X).
rk(X) :-
    X = a.
