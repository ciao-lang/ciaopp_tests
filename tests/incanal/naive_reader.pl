:- module(naive_reader, [read_module/2, clean_reader/0], [assertions, datafacts]).

:- doc(title, "Naive reader of Ciao modules").

:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "This module assumes that all directives are present at
    the beginning of the module.").

:- use_module(engine(stream_basic)).
:- use_module(library(read), [read/2]).
:- use_module(library(aggregates), [findall/3, bagof/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(operators)).
:- use_module(config_db, [get_file_config/1]).

:- use_module(library(hiordlib),[foldl/4]).

:- export(module_clause/6).
:- doc(module_clause/6, "A clause writen in a module, it is of the form
@tt{module_clause(Mod, ClType, Pred, A, ClN, Head, Body)}.").
:- data module_clause/6.
% module_clause(Mod, ClType, Pred, A, ClN, Clause).

:- export(naive_loaded/2).
:- pred naive_loaded(Mod,ModPath).
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
    retractall_fact(module_clause(Mod, _, _, _, _, _)),
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
    assertz_fact(module_clause(Mod, module, _, _, _, Clause)).
process(Mod, Clause) :-
    (Clause = (:- use_module(_, _)) ;
     Clause = (:- use_module(_))
    ), !,
    assertz_fact(module_clause(Mod, directive, _, _, _, Clause)).
process(Mod, Clause) :-
    Clause = ':-'(data(_)), !,
    assertz_fact(module_clause(Mod, directive, _, _, _, Clause)).
process(Mod, Clause) :- % entries
    Clause = ':-'(Decl),
    Decl = entry(Head:_Call), !,
    functor(Head, P, A),
    assertz_fact(module_clause(Mod, assertion, P, A, _, Clause)).
process(Mod, Clause) :- % directives
    Clause = ':-'(_), !,
    assertz_fact(module_clause(Mod, directive, _, _, _, Clause)).
process(Mod, Clause) :- % clauses
    is_clause(Clause, P, A), !,
    assertz_fact(module_clause(Mod, clause, P, A, _, Clause)).

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
:- pred get_code_summary(Sum, N) #"Summarizes the number of clauses
present in each of the modules loaded in the database. The format of
@var{Sum} is compatible with the input of
edition_sequence_generator.".
get_code_summary(Sum,N) :-
    ( get_file_config(edition_mode(predicate)) ->
        get_preds_summary(Sum, N)
    ;
        get_cls_summary(Sum, N)
    ).

get_cls_summary(Sum, NCls) :-
    findall(Cl, module_clause(_, clause, _, _, _, Cl), Cls),
    length(Cls, NCls),
    findall(Mod, naive_loaded(Mod, _), Mods),
    get_mods_summary(Mods, Sum).

get_mods_summary([], []).
get_mods_summary([Mod|Mods], [Mod-N|Sum]) :-
    findall(Cl, module_clause(Mod, clause, _, _, _, Cl), Cls),
    length(Cls, N),
    get_mods_summary(Mods, Sum).

get_preds_summary(PredSum,NPreds) :-
    findall(Mod, naive_loaded(Mod, _), Mods),
    findall(M-PS, (member(M,Mods), summ_preds_mod(M,PS)), PredSum),
    foldl(add_mods,PredSum,0,NPreds).

summ_preds_mod(Mod,PS) :-
    findall(Pred/A-N,
            (bagof(Cl, (module_clause(Mod,clause,Pred,A,_,Cl)),PredSum),
             length(PredSum, N)),
            PS0),
    reorder_summary(PS0,Mod,PS).

:- data proc_pred/3.
% reorder predicates to keep order in source -- lost because bagof sorts
reorder_summary(Sum,Mod,Sum_s) :-
    retractall_fact(proc_pred(_,_,_)),
    ( % failured-driven loop
      module_clause(Mod,clause,Pred,A,_,_),
        get_member(Pred/A-N, Sum),
        ( proc_pred(Pred,A,N) -> true
        ; assertz_fact(proc_pred(Pred,A,N))
        ),
        fail
    ; findall(P/A-N, proc_pred(P,A,N), Sum_s)
    ).

get_member(A,L) :-
    member(A,L), !.

add_mods(_-Ps,V0,V):-
    length(Ps, L),
    V is V0 + L.

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
    op(1105, xfy, ('|')),
    % fsyntax
    op(1150,  fx, (fun_eval)),
    op(1150,  fx, (fun_return)),
    op(1130, xfx, (:=)),
    op(  50,  fx, (~)),
    op( 910,  fx, (^^)),
    op(  25,  fy, (^)),
    op(1105, xfy, ('|')),
    op(1050, xfy, (?)).
