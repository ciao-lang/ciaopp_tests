:- module(_, [main/0], []).

:- use_module(library(lists), [length/2]).

:- use_module(.(browse), [investigate/2]).

main :-
    init(100,10,4,
            [[a,a,a,b,b,b,b,a,a,a,a,a,b,b,a,a,a],
             [a,a,b,b,b,b,a,a,[a,a],[b,b]],
             [a,a,a,b,[b,a],b,a,b,a]
            ],
            Symbols),
    randomize(Symbols,RSymbols,21),!,
    investigate(RSymbols,
            [[star(SA),B,star(SB),B,a,star(SA),a,star(SB),star(SA)],
             [star(SA),star(SB),star(SB),star(SA),[star(SA)],[star(SB)]],
             [_X,_Y,star(_Z),[b,a],star(_A),_B,_C]
            ]).

init(N,M,Npats,Ipats,Result) :- init_(N,M,M,Npats,Ipats,Result).

init_(0,_X,_Y,_Z,_W,_L) :- !.
init_(N,I,M,Npats,Ipats,[Symb|Rest]) :- 
    fill(I,[],L),
    get_pats(Npats,Ipats,Ppats),
    J is M - I,
    fill(J,[pattern(Ppats)|L],Symb),
    N1 is N - 1,
    (I == 0 -> I1 is M; I1 is I - 1),
    init_(N1,I1,M,Npats,Ipats,Rest).

fill(0,L,L) :- !.
fill(N,L,[dummy([])|Rest]) :- N1 is N - 1, fill(N1,L,Rest).

randomize([],[],_) :- !.
randomize(In,[X|Out],Rand) :-
    length(In,Lin),
    Rand1 is (Rand * 17) mod 251,
    N is Rand1 mod Lin,
    split(N,In,X,In1),
    randomize(In1,Out,Rand1).

split(0,[X|Xs],X,Xs) :- !.
split(N,[X|Xs],RemovedElt,[X|Ys]) :-
    N1 is N - 1,
    split(N1,Xs,RemovedElt,Ys).


get_pats(Npats,Ipats,Result) :- get_pats_(Npats,Ipats,Result,Ipats).

get_pats_(0,_X,[],_Y) :- !.
get_pats_(N,[X|Xs],[X|Ys],Ipats) :-
    N1 is N - 1,
    get_pats_(N1,Xs,Ys,Ipats).
get_pats_(N,[],Ys,Ipats) :-
    get_pats_(N,Ipats,Ys,Ipats).
