:- module(_, [p_investigate/2], []).

p_investigate([],_X).
p_investigate([D|Data],Patterns) :-
    p_match(Patterns,D),
    p_investigate(Data,Patterns).

p_match([],_).
p_match([P|Patterns],D) :-
    (match(D,P), fail; true),
    p_match(Patterns, D).

match([],[]) :- !.
match([X|PRest],[Y|SRest]) :-
    var(Y),!,X = Y,
    match(PRest,SRest).
match(List,[Y|Rest]) :- 
    nonvar(Y),Y = star(X),!,
    concat(X,SRest,List),
    match(SRest,Rest).
match([X|PRest],[Y|SRest]) :-
    (atom(X) -> X = Y; match(X,Y)),
    match(PRest,SRest).

concat([],L,L).
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).
