:- module(progeom,[pds/2, pdsbm/1],[]).
%------------------------------------------------------------------------------
%       Benchmark Program - Projective Geometry
%
%       by William Older
%       Date: 
%
%       To run: pds(N,Out). or pdsbm(N).
%------------------------------------------------------------------------------

/*  
    program pds[n,_list] attempts to construct a perfect difference
    set  ( in increasing order, starting at 0) of order n.  Such a set
    determines a finite cyclic projective geometry with n(n+1)+1 points.
    There are answers known for n=2,3,4,5, and 7. (not sure about 8,9)
    The case n=10 is an open question.

    This is version 2 with an optimized version of the check routines.

    The exported predicate is pds/2, with mode [++, nv].

*/

:- use_module(my_library).

pdsbm(N):- pds(N, [0,1|_]).

pds(Order, [0|Ans]):-
    N is Order * (Order + 1) + 1,
    iota(N, [0|List]),
    pds1(List, [0], Ans, N).

pds1([],_,[],_):-!.
pds1(Unused, List, [Choice|Rest],Mod):-
    my_library:member(Choice,Unused),
    check(List, Choice, Unused,U3,Mod),
    pds1(U3, [Choice| List], Rest, Mod).

iota(N,List):-
    iota1(0,N,List).   %origin 0
iota1(K,K,[]):-!.
iota1(K,N,[K|List]):-
    K1 is K+1,
    iota1(K1,N,List).

check([],_,L,L,_):-!.
check(S, Choice, Old, L3,Modulus):-
    dif(S, Choice,Modulus, Ds,Dds),
    mergedelete(Ds, Old,L2),
    rev(Dds,[],Rds),
    mergedelete(Rds, L2,L3),
    !.
