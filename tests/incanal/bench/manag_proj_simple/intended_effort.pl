:- module(intended_effort,[intended_effort/5,total_amos/1],[assertions]).

:- doc(intended_effort(Project,WP,Task,Partner,MM), "@var{Partner}
intends to devote @var{MM} man moths to @var{Task} in @var{WP} of
@var{Project}").

:- discontiguous intended_effort/5.

:- doc(intended_effort(Project,WP,ID,Partner,Effort),
"@var{Effort} is the number of MMs which @var{Partner} intends to
devote to task @var{ID} within @var{WP} of @var{Project}.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_package(datafacts).
:- data d/1.
my_findall(Tmpl, G, Xs) :-
    asserta_fact(d(first)),
    my_collect(Tmpl, G),
    my_sols([],Xs0), Xs=Xs0.
my_collect(Tmpl, G) :-
    ( my_call(G),
      asserta_fact(d(sol(Tmpl))),
      fail
    ; true
    ).
my_sols(Xs0,Xs) :-
    retract_fact(d(Mark)),
    !,
    ( Mark = first -> Xs = Xs0
    ; Mark = sol(X) -> Xs1 = [X|Xs0], my_sols(Xs1, Xs)
    ).
my_call(intended_effort(A,B,C,D,E)) :- intended_effort(A,B,C,D,E).

%WP1 manag
intended_effort(asap,1,1,1,2).
intended_effort(amos,2,1,1,2).

intended_effort(colognet,2,german,1,X):- X is 210/131.25. 

total_amos(X):-
    Prometidos is 21.7,
    my_findall(X,intended_effort(amos,2,_,_,X),List_Effort),
    subtract_list(List_Effort,Prometidos,X).

subtract_list([],P,P).
subtract_list([X|Xs],P,NP):-
    subtract_list(Xs,P,Tmp),
    NP is Tmp - X.
