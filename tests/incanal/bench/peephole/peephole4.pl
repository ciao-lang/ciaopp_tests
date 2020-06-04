:- module(_,[popt4/3],[]).

:- use_module(peephole_lib).

popt4([],_1,[]).
popt4([label((P,N,K))|Rest],Seen,[label((P,N,K))|ORest]) :-
    !,
    'popt4/3/2/$disj/1'(Seen,P,N),
    popt4(Rest,Seen,ORest).
popt4([execute((P,N)),label((P,N,K))|Rest],Seen,OList) :-
    !,
    'popt4/3/3/$disj/1'(Seen,OList,P,N,K,ORest),
    popt4(Rest,Seen,ORest).
popt4([Inst|Rest],Seen,[Inst|ORest]) :-
     popt4(Rest,Seen,ORest).

'popt4/3/2/$disj/1'(Seen,P,N) :-
    N>=0,
    member1((P,N),Seen).
'popt4/3/2/$disj/1'(_,_,N) :-
    N<0.

'popt4/3/3/$disj/1'(Seen,OList,P,N,K,ORest) :-
    N>=0,
    popt_chkmember((P,N),Seen,SFlag),
    'popt4/3/3/$disj/1/6/1/$disj/1'(OList,P,N,K,ORest,SFlag).
'popt4/3/3/$disj/1'(_Seen,OList,P,N,K,ORest) :-
    N<0,
    OList=[execute((P,N)),label((P,N,K))|ORest].

'popt4/3/3/$disj/1/6/1/$disj/1'(OList,P,N,K,ORest,SFlag) :-
    SFlag=:=1,
    OList=[execute((P,N)),label((P,N,K))|ORest].
'popt4/3/3/$disj/1/6/1/$disj/1'(OList,P,N,K,ORest,SFlag) :-
    SFlag=\=1,
    OList=[label((P,N,K))|ORest].



