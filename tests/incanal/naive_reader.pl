:- module(naive_reader, [read_module/2, clean_reader/0], [assertions, datafacts]).

:- use_module(engine(stream_basic)).
:- use_module(library(read), [read/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(operators)).

:- doc(title, "Naive reader of Ciao modules").

:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "This module assumes that all directives are present at
	the beginning of the module.").

:- export(module_clause/6).
:- doc(module_clause/6, "A clause writen in a module, it is of the form
 module_clause(ClType, Pred, A, Mod, ClN, Head, Body).").
:- data module_clause/6.
% module_clause(ClType, Pred, A, Mod, ClN, Clause).

:- data naive_loaded/2.

% do this before loading new directories
:- doc(clean_reader/0, "Cleans the reader (previously readed clauses).").
clean_reader :-
	retractall_fact(module_clause(_, _, _, _, _, _)),
	retractall_fact(naive_loaded(_, _)).

:- pred read_module(Mod, ModPath) : atm * atm #"Reads a module
@var{Mod} located in @var{ModPath}. The previous information stored
for module @var{Mod} will be cleaned.".
read_module(Mod, ModPath) :-
	runtime_ops,
	retractall_fact(module_clause(_, _, _, Mod, _, _)),
	open(ModPath, read, S),
	assertz_fact(naive_loaded(Mod, ModPath)),
	repeat,
	read(S,X),
	(X = end_of_file, !
	    ; process(Mod, X),
	      fail),
	close(S).

process(Mod, Clause) :-
	(Clause = (:- module(_, _, _)) ; Clause = (:- module(_, _))), !,
	assertz_fact(module_clause(module, _, _, Mod, _, Clause)).
process(Mod, Clause) :-
	(Clause = (:- use_module(_, _)) ;
	 Clause = (:- use_module(_))
	), !,
	assertz_fact(module_clause(directive, _, _, Mod, _, Clause)).
process(Mod, Clause) :-
	Clause = ':-'(data(_)), !,
	assertz_fact(module_clause(directive, _, _, Mod, _, Clause)).
process(Mod, Clause) :- % entries
	Clause = ':-'(Decl),
	Decl = entry(Head:_Call), !,
	functor(Head, P, A),
	assertz_fact(module_clause(assertion, P, A, Mod, _, Clause)).
process(Mod, Clause) :- % directives
	Clause = ':-'(_), !,
	assertz_fact(module_clause(directive, _, _, Mod, _, Clause)).
process(Mod, Clause) :- % clauses
	is_clause(Clause, P, A), !,
	assertz_fact(module_clause(clause, P, A, Mod, _, Clause)).

% Detect if a term corresponds do a clause and obtain the predicate
% name and arity (simulating some of the syntactic extensions)
is_clause('-->'(Head, _Body), P, A) :- !, % DCGs
	functor(Head, P, A0),
	A is A0 + 2.
is_clause((':='(Head,  _) :- _Body), P, A) :- !, % fsyntax
	functor(Head, P, A0),
	A is A0 + 1.
is_clause(':='(Head, _), P, A) :- !, % fsyntax
	functor(Head, P, A0),
	A is A0 + 1.
is_clause((Head :- _Body), P, A) :- !, % normal clause
	functor(Head, P, A).
is_clause(Head, P, A) :- !, % a fact
	functor(Head, P, A).

:- export(get_code_summary/2).
:- pred get_code_summary(Sum, NCls) #"Summarizes the number of clauses
present in each of the modules loaded in the database. The format of
@var{Sum} is compatible with the input of
edition_sequence_generator.".
get_code_summary(Sum, NCls) :-
	findall(Cl, module_clause(clause, _, _, _, _, Cl), Cls),
	length(Cls, NCls),
	findall(Mod, naive_loaded(Mod, _), Mods),
	get_mods_summary(Mods, Sum).

get_mods_summary([], []).
get_mods_summary([Mod|Mods], [Mod-N|Sum]) :-
	findall(Cl, module_clause(clause, _, _, Mod, _, Cl), Cls),
	length(Cls, N),
	get_mods_summary(Mods, Sum).

% assertions, fsyntax, dcg, regtypes
runtime_ops :-
	% assertions
	op(1150, fx,(entry)),
	op(975, xfx,(=>)),
	op(978, xfx,(::)),
	op(1150, fx,(pred)),
        op(1150,xfx,(pred)),
	op(1150,xfx,(success)),
	op(1150,xfx,(calls)),
	% isomodes
	op(200, fy, [(?),(@)]),
	% regtypes
	op(1150, fx,(regtype)),
	op(1150,xfx,(regtype)),
	% dcg
	op(1200, xfx,[(-->)]),
	op(1100, xfy, ('|')),
	% fsyntax
	op(1150,  fx, (function)),
	op(1150,  fx, (fun_eval)),
	op(1150,  fx, (fun_return)),
	op(1130, xfx, (:=)),
	op(  50,  fx, (~)),
	op( 910,  fx, (^^)),
	op(  25,  fy, (^)),
	op(1100, xfy, ('|')),
	op(1050, xfy, (?)).

