:- module(_,[main2/1],[]).

:- use_module(bu_test_aux).

% this is to test external calls
%% main(X) :-
%%     % the analysis result is unnaffected if a rec cl is deleted
%%     p(X).

%% % this is check that only the rec clauses are deleted
%% p(a).
%% p(b).
%% p(c).
%% p(X) :- X = d.
%% p(X) :-
%%     q(X).

%% q(1).
%% q(2).
%% q(3).
%% q(X) :- X = 4.
%% q(X) :-
%%     r(X).

%% r('A').
%% r('B').
%% r('C').
%% r(X) :- X = 'D'.
%% r(X) :-
%%     p(X).


main2(X) :-
    a(X).

a(X) :- b(X).
b(X) :- c(X).

c(X) :- aux(X).
c(X) :- a(X).
