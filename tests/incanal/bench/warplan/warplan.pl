%------------------------------------------------------------------------------
%       Benchmark Program - (war) plan for robot control
%
%       by D.H.D Warren
%       Date: 
%
%       To test: try test1. test2. test3. or test4.
%------------------------------------------------------------------------------
%:- entry(plans(g,g)).
:- module(warplan,[plans/2],[]).

% modularized by IG

:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(engine(io_basic)).
:- use_module(library(write), [write/1]).
:- use_module(library(sort), [sort/2]).

:- use_module(robot_operations, [can/2, add/2, del/2]).

:- op(900,yfx,::). % TODO:

plans(C,_) :-
    not(consistent(C,true)), !,
    nl, write('impossible'), nl.
plans(C,T) :-
    time(M0),
    plan(C,true,T,T1),
    time(M1), nl,
    output(T1), nl,
    Time is (M1-M0)/1000,
    write(Time),
    write(' secs.'), nl.

time(T) :-
    statistics(runtime,[T,_]).

output(T::U) :- !,
    output1(T),
    write(U),
    write('.'), nl.
output(T) :-
    write(T),
    write('.'), nl.

output1('!!'(T,U)) :- !,
    output1(T),
    write(U),
    write(';'),
    nl.
output1(T) :-
    write(T),
    write(';'), nl.

plan('##'(X,C),P,T,T2) :- !,
    solve(X,P,T,P1,T1),
    plan(C,P1,T1,T2).
plan(X,P,T,T1) :- solve(X,P,T,_,T1).

solve(X,P,T,P,T) :-
    always(X).
solve(X,P,T,P1,T) :-
    holds(X,T),
    and(X,P,P1).
solve(X,P,T,'##'(X,P),T1) :-
    add(X,U),
    achieve(X,U,P,T,T1).

achieve(_,U,P,T,T1::U ) :- 
    preserves(U,P),
    can(U,C),
    consistent(C,P),
    plan(C,P,T,T1),
    preserves(U,P).
achieve(X,U,P,T::V,T1::V) :- 
    preserved(X,V),
    retrace(P,V,P1),
    achieve(X,U,P1,T,T1),
    preserved(X,V).

holds(X,_::V) :-
    add(X,V).
holds(X,T::V) :- !, 
    preserved(X,V),
    holds(X,T),
    preserved(X,V).
holds(X,T) :-
    given(T,X).

preserved(X,V) :- mkground('##'(X,V),0,_), del(X,V), !, fail.
preserved(_,_).

preserves(U,'##'(X,C)) :- preserved(X,U), preserves(U,C).
preserves(_,true).

retrace(P,V,P2) :- 
    can(V,C),
    retrace1(P,V,C,P1),
    conjoin(C,P1,P2).

retrace1('##'(X,P),V,C,P1) :-
    add(Y,V),
    equiv(X,Y), !,
    retrace1(P,V,C,P1).
retrace1('##'(X,P),V,C,P1) :-
    elem(Y,C),
    equiv(X,Y), !,
    retrace1(P,V,C,P1).
retrace1('##'(X,P),V,C,'##'(X,P1)) :-
    retrace1(P,V,C,P1).
retrace1(true,_,_,true).

consistent(C,P) :- 
    mkground('##'(C,P),0,_),
    imposs(S),
    not(not(intersect(C,S))),
    implied(S,'##'(C,P)), 
    !, fail.
consistent(_,_).

not(X) :-
    my_call(X), !, fail.
not(_).

my_call(intersect(A,B)) :-
    intersect(A,B).
my_call(nonequiv(X,Y)) :-
    nonequiv(X,Y).
my_call(A=B) :-
    A=B.
my_call(consistent(A,B)) :-
    consistent(A,B).
my_call(not(X)) :-
    not(X).

imposs(sometimes).

and(X,P,P) :-
    elem(Y,P),
    equiv(X,Y), !.
and(X,P,'##'(X,P)).

conjoin('##'(X,C),P,'##'(X,P1)) :- !,
    conjoin(C,P,P1).
conjoin(X,P,'##'(X,P)).

elem(X,'##'(Y,_)) :-
    elem(X,Y).
elem(X,'##'(_,C)) :- !,
    elem(X,C).
elem(X,X).

intersect(S1,S2) :-
    elem(X,S1),
    elem(X,S2).

implied('##'(S1,S2),C) :- !,
    implied(S1,C),
    implied(S2,C).
implied(X,C) :- elem(X,C).
%% implied(X,_) :- call(X).

notequal(X,Y) :- 
    not(X=Y),
    not(X=qqq(_)),
    not(Y=qqq(_)).

equiv(X,Y) :-
    not(nonequiv(X,Y)).

nonequiv(X,Y) :-
    mkground('##'(X,Y),0,_),
    X=Y, !, fail.
nonequiv(_,_).

mkground(qqq(N1),N1,N2) :- !,
    N2 is N1+1.
mkground(qqq(_),N1,N1) :- !.
mkground(X,N1,N2) :-
    X =.. [_|L],
    mkgroundlist(L,N1,N2).

mkgroundlist([X|L],N1,N3) :-
    mkground(X,N1,N2),
    mkgroundlist(L,N2,N3).
mkgroundlist([],N1,N1).

always( connects(D,R1,R2)) :-
    connects1(D,R1,R2).
always( connects(D,R2,R1)) :-
    connects1(D,R1,R2).
always( inroom(D,R1)) :-
    always(connects(D,_,R1)).
always( pushable(box(_))).
always( locinroom(point(6),room(4))).
always( inroom(lightswitch(1),room(1))).
always( at(lightswitch(1),point(4))).

connects1(door(N),room(N),room(5)) :-
    range(N,1,4).

range(M,M,_).
range(M,L,N) :- L < N, L1 is L+1, range(M,L1,N).

given( start, at(box(N), point(N))) :-
    range(N,1,3).
given( start, at(robot,point(5))).
given( start, inroom(box(N),room(1))) :-
    range(N,1,3).
given( start, inroom(robot,room(1))).
given( start, onfloor).
given( start, status(lightswitch(1),off)).
