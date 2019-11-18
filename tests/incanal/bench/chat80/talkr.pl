:- module(talkr, [answer/2, dlst/3, satisfy/1, write_tree/1], [assertions]).
/* Simplifying and executing the logical form of a NL query. */
% Changed answer to return output in an argument. M.H.

:- use_module(library(aggregates), [setof/3]).
:- use_module(library(lists), [length/2]).
:- use_module(engine(io_basic)).
:- use_module(library(write), [numbervars/3, write/1]).

:- use_module(world0, [database/1]).

%:- include(chatops).

/*
:-mode write_tree(+).
:-mode wt(+,+).
:-mode header(+).
:-mode decomp(+,-,-).
:-mode complex(+).
:-mode othervars(+,-,-).
*/


:- trust calls write_tree(X) : gnd(X).
:- trust calls wt(X,Y) : (gnd(X), gnd(Y)).
:- trust calls header(X) : gnd(X).
:- trust calls decomp(X,Y,Z) : (gnd(X), var(Y), var(Z)).
:- trust calls complex(X) : gnd(X).
:- trust calls othervars(X,Y,Z) : (gnd(X), var(Y), var(Z)).


write_tree(T):-
   numbervars(T,0,_),
   wt(T,0),
   fail.
write_tree(_).

wt((P:-Q),L) :- !, L1 is L+3,
   write(P), tab(1), write((:-)), nl,
   tab(L1), wt(Q,L1).
wt((P,Q),L) :- !, L1 is L-2,
   wt(P,L), nl,
   tab(L1), put("&"), tab(1), wt(Q,L).
wt({P},L) :- complex(P), !, L1 is L+2,
   put("{"), tab(1), wt(P,L1), tab(1), put("}").
wt(E,L) :- decomp(E,H,P), !, L1 is L+2,
   header(H), nl,
   tab(L1), wt(P,L1).
wt(E,_) :- write(E).

header([]).
header([X|H]) :- write(X), tab(1), header(H).

decomp(setof(X,P,S),[S,=,setof,X],P).  
decomp(\+(P),[\+],P) :- complex(P).
decomp(numberof(X,P,N),[N,=,numberof,X],P).
decomp(X^P,[exists,X|XX],P1) :- othervars(P,XX,P1).

othervars(X^P,[X|XX],P1) :- !, othervars(P,XX,P1).
othervars(P,[],P).

complex((_,_)).
complex({_}).
complex(setof(_,_,_)).
complex(numberof(_,_,_)).
complex(_^_).
complex(\+P) :- complex(P).

% Query execution.

/*
:-mode respond(?).
:-mode holds(+,?).
:-mode answer(+).
:-mode yesno(+).         :-mode replies(+).
:-mode reply(+).
:-mode seto(?,+,-).
:-mode satisfy(+).
:-mode pickargs(+,+,+).
:-mode pick(+,?).
*/

% :- trust calls respond(?). % Not necessary
:- trust calls holds(X,?) : (gnd(X)).
:- trust calls answer(X,_) : (gnd(X)).
:- trust calls yesno(X,_) : (gnd(X)).
:- trust calls replies(X,_,_) : (gnd(X)).
:- trust calls reply(X,_) : (gnd(X)).
:- trust calls seto(_,X,Y) : (gnd(X), var(Y)).
:- trust calls satisfy(X) : (gnd(X)).
:- trust calls pickargs(X,Y,Z) : (gnd(X), var(Y), var(Z)).
:- trust calls pick(X,_) : (gnd(X)).


respond([],"Nothing satisfies your question.").
respond([A|L],Ans) :- 
    reply(A,Ans,Ansrs), 
    replies(L,Ansrs,[]).
%% respond([]) :- write('Nothing satisfies your question.'), nl.
%% respond([A|L]) :- reply(A), replies(L).

answer((answer([]):-E),A) :- !, holds(E,B), yesno(B,A).
answer((answer([X]):-E),A) :- !, seto(X,E,S), respond(S,A).
answer((answer(X):-E),A) :- seto(X,E,S), respond(S,A).

seto(X,E,S) :- setof(X,satisfy(E),S), !.
seto(_X,_E,[]).

holds(E,true) :- satisfy(E), !.
holds(_E,false).

yesno(true,"Yes."). 
yesno(false,"No."). 
%% yesno(true) :- write('Yes.').
%% yesno(false) :- write('No.').

replies([],[0'.|R],R).
replies([A],Ans,Ansrs) :- 
    dlst(" and ",Ans,Q1),
    reply(A,Q1,Q2), 
    Q2 = [0'.|Ansrs].
replies([A|X],Ans,Ansrs) :- 
    dlst(", ",Ans,Q1),
    reply(A,Q1,Q2), 
    replies(X,Q2,Ansrs).
%% replies([]) :- write('.').
%% replies([A]) :- write(' and '), reply(A), write('.').
%% replies([A|X]) :- write(', '), reply(A), replies(X).

reply('--'(N,A),As) :- 
    !, 
    name(N,Ns),
    dlst(Ns,A,[0' |Q1]),
    name(_U,Us),
    dlst(Us,Q1,As).
reply(X,A,As) :- 
    name(X,Xs),
    dlst(Xs,A,As).
%% reply(N--U) :- !, write(N), write(' '), write(U).
%% reply(X) :- write(X).

satisfy((P,Q)) :- !, satisfy(P), satisfy(Q).
satisfy({P}) :- !, satisfy(P), !.
satisfy(_X^P) :- !, satisfy(P).
satisfy(\+P) :- satisfy(P), !, fail.
satisfy(\+_P) :- !.
satisfy(numberof(X,P,N)) :- !, setof(X,satisfy(P),S), length(S,N).
satisfy(setof(X,P,S)) :- !, setof(X,satisfy(P),S).
satisfy(+P) :- exceptionto(P), !, fail.
satisfy(+_P) :- !.
satisfy(X<Y) :- !, X<Y.
satisfy(X=<Y) :- !, X=<Y.
satisfy(X>=Y) :- !, X>=Y.
satisfy(X>Y) :- !, X>Y.
satisfy(P) :- database(P).

exceptionto(P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception(P1).

exception(P) :- database(P), !, fail.
exception(_P).

pickargs(0,_,_) :- !.
pickargs(N,P,P1) :- N1 is N-1,
   arg(N,P,S),
   pick(S,X),
   arg(N,P1,X),
   pickargs(N1,P,P1).

pick([X|_S],X).
pick([_|S],X) :- !, pick(S,X).
pick([],_) :- !, fail.
pick(X,X).

dlst([X|Y],[X|R],E) :-
    !,
    dlst(Y,R,E).
dlst([],E,E).

%% PBC
put([C]):- !, put_code(C).
put(C):- put_code(C).
