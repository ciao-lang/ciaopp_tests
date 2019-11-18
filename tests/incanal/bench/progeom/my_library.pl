:- module(my_library, _, []).

dif([],_,_,[],[]).
dif([S|Ss],Val,Mod,[D|Ds],[D2|D2s]):-
     D is Val - S, D2 is Mod - D,
     dif( Ss,Val,Mod,Ds,D2s).
    
rev([],L,L).
rev([X|Xs], Y,L):-
    rev(Xs,[X|Y],L).

member(X, [X|Xs]).
member(X, [_|Xs]):- member(X,Xs).

mergedelete([],L,L).
mergedelete([D|Ds], [D|R], L2):-
    mergedelete(Ds, R, L2).
mergedelete([D|Ds], [X|R], [X|L2]):-
    D @> X,
    mergedelete([D|Ds],R,L2).

