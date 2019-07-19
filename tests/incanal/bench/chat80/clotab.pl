:- module(clotab,
	[
	  adv/1,
	  compl_case/1,
	  empty/1,
	  is_adv/1,
	  is_pp/1,
	  is_pred/1,
	  minus/3,
	  np_all/1,
	  np_no_trace/1,
	  plus/3,
	  prep_case/1,
	  role/3,
	  s_all/1,
	  subj_case/1,
	  trace/1,
	  trace/2,
	  verb_case/1
	],
	[assertions]).

% Normal form masks

is_pp(#(1,_,_,_)).

is_pred(#(_,1,_,_)).

is_trace(#(_,_,1,_)).

is_adv(#(_,_,_,1)).

:- trust success trace(_,A) => (gnd(A)).
trace(#(_,_,1,_),#(0,0,0,0)).

:- trust success trace(A) => (gnd(A)).
trace(#(0,0,1,0)).

:- trust success adv(A) => (gnd(A)).
adv(#(0,0,0,1)).

:- trust success empty(A) => (gnd(A)).
empty(#(0,0,0,0)).

:- trust success np_all(A) => (gnd(A)).
np_all(#(1,1,1,0)).

:- trust success s_all(A) => (gnd(A)).
s_all(#(1,0,1,1)).

:- trust success np_no_trace(A) => (gnd(A)).
np_no_trace(#(1,1,0,0)).

% Mask operations


plus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   or(B1,C1,D1),
   or(B2,C2,D2),
   or(B3,C3,D3),
   or(B4,C4,D4).

minus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   anot(B1,C1,D1),
   anot(B2,C2,D2),
   anot(B3,C3,D3),
   anot(B4,C4,D4).

or(1,_,1).
or(0,1,1).
or(0,0,0).

anot(X,0,X).
anot(_X,1,0).

% Noun phrase position features

:- trust success role(A,_,_) => (gnd(A)).
role(subj,_,#(1,0,0)).
role(compl,_,#(0,_,_)).
role(undef,main,#(_,0,_)).
role(undef,aux,#(0,_,_)).
role(undef,decl,_).
role(nil,_,_).

:- trust success subj_case(A) => (gnd(A)).
subj_case(#(1,0,0)).

:- trust success verb_case(A) => (gnd(A)).
verb_case(#(0,1,0)).

:- trust success prep_case(A) => (gnd(A)).
prep_case(#(0,0,1)).

compl_case(#(0,_,_)).
