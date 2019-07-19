:- module(main_leancop, [main/1], [datafacts]).

%% File: main_leancop.pl  -  Version: 1.0  -  Date: 3 July 2009
%%
%% Purpose: Call the leanCoP core prover for a given formula
%%
%% Authors: Jens Otten
%% Web:     www.leancop.de
%%
%% Usage:   leancop_main(X,S,R). % proves formula in file X with
%%                               %  settings S and returns result R
%%
%% Copyright: (c) 2009 by Jens Otten
%% License:   GNU General Public License

:- use_module(engine(io_basic)).
:- use_module(library(system)).  % load system module
:- use_module(library(lists), [append/3]).  % load system module
:- use_module(library(read_from_string), [read_from_atom/2]).

:- use_module(leancop21, [prove2/3]).
:- use_module(def_mm, [make_matrix/3]).
:- use_module(leancop_proof, [set_proof_layout/1, leancop_proof/2]).
:- use_module(leancop_tptp2, [leancop_tptp2/5, leancop_equal/2]).

main([Layout, File, Settings0]) :-
	set_axiom_path,
	read_from_atom(Settings0, Settings),
	set_proof_layout(Layout),
	leancop_main(File, Settings, _Result).

:- data axiom_path/1.

set_axiom_path :-
	( getenvstr('TPTP',Path) -> atom_codes(Path1,Path) ; Path1='' ),
	retractall_fact(axiom_path(_)),
	asserta_fact(axiom_path(Path1)).

% call leanCoP core prover
leancop_main(File,Settings,Result) :-
	axiom_path(AxPath),
	( AxPath='' -> 
	    AxDir=''
	; name(AxPath,AxL),
	  append(AxL,"/",DirL),
	  name(AxDir,DirL)
	),
	( leancop_tptp2(File,AxDir,[_],F,Conj) ->
	    Problem=F
	; throw(cannot_read_tptp2(File)) /*[File], f(Problem), Conj=non_empty*/
	),
	( Conj\=[] -> Problem1=Problem ; Problem1=('~'(Problem)) ),
	leancop_equal(Problem1,Problem2),
	make_matrix(Problem2,Matrix,Settings),
	( prove2(Matrix,Settings,Proof) ->
	    ( Conj\=[] -> Result='Theorem' ; Result='Unsatisfiable' )
	; ( Conj\=[] -> Result='Non-Theorem' ; Result='Satisfiable' )
	),
	output_result(File,Matrix,Proof,Result,Conj).

% print status and connection proof
output_result(File,Matrix,Proof,Result,Conj) :-
    ( Conj\=[] -> Art=' is a ' ; Art=' is ' ), nl,
    print(File), print(Art), print(Result), nl,
    Proof=[] -> true ; ( Result='Theorem' -> Out=' for ' ;
      Result='Unsatisfiable' -> Out=' for negated ' ; true ),
    ( var(Out) -> true ; print('Start of proof'), print(Out),
      print(File), nl, leancop_proof(Matrix,Proof),
      print('End of proof'), print(Out), print(File), nl ).

print(X) :- display(X).
