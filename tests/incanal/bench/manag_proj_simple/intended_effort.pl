:- module(intended_effort,[intended_effort/5,total_amos/1],[assertions]).

:- use_module(library(aggregates), [findall/3]).

:- doc(intended_effort(Project,WP,Task,Partner,MM), "@var{Partner}
intends to devote @var{MM} man moths to @var{Task} in @var{WP} of
@var{Project}").

:- discontiguous intended_effort/5.

:- doc(intended_effort(Project,WP,ID,Partner,Effort),
"@var{Effort} is the number of MMs which @var{Partner} intends to
devote to task @var{ID} within @var{WP} of @var{Project}.").

%WP1 manag
intended_effort(asap,1,1,1,2).
intended_effort(amos,2,1,1,2).

intended_effort(colognet,2,german,1,X):- X is 210/131.25. 

total_amos(X):-
	Prometidos is 21.7,
	findall(X,intended_effort(amos,2,_,_,X),List_Effort),
	subtract_list(List_Effort,Prometidos,X).

subtract_list([],P,P).
subtract_list([X|Xs],P,NP):-
	subtract_list(Xs,P,Tmp),
	NP is Tmp - X.
