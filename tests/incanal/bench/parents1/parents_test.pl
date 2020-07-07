:- module(_,[main/2],[]).

p(A,B,C) :-
    main(D,E).

main([],[]).
main([Inst|Rest],Pil) :-
    not_p(Inst,Rest,Pil),
    main(Pil,Pil1).

not_p(Inst,Rest,Pil) :-
    p(Inst,Rest,Pil), !, fail.
not_p(_,_,_).
