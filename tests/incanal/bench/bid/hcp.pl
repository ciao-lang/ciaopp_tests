:- module(hcp,[hcp/3],[]).

:- use_module(honors, [honors/3]).
:- use_module(misc, [misc/2]).

/*
    hcp(H,Si,So) -- count the high-card-points in hand H
*/

hcp([],N,N).
hcp([_X=C|Sn],Ni,No) :-
    hcp_suit(C,N) ,
    Nt is N + Ni ,
    hcp(Sn,Nt,No).

hcp_suit(S,P) :-
    honors(S,0,HP) ,
    dist(S,DP) ,
    misc(S,MP) ,
    P is HP + DP + MP.

dist([],3).
dist([_],2).
dist([_,_],1).
dist([_,_,_|_],0).

