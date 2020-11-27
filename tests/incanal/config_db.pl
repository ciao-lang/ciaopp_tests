:- module(_,[],[assertions,datafacts]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(title, "Experiment & bundle configuration db").

:- use_module(library(terms_io), [file_to_terms/2]).

:- export(monolithic/0).
:- doc(monolithic/0, "Flag to store whether the analysis is monolithic or not.").
:- data monolithic/0.

:- data test_config/2.
:- export(test_config/2).
% default opts
test_config('--group', clause). % clause ; pred 
test_config('--one_mod', yes). % yes ; no
test_config('--seq_sz', abs). % abs ; rel
%test_config('rand', yes). % yes ; no
test_config('--edit_type', add). % add ; del
test_config('--n_edits', 1). % int
test_config('--domain', shfr).
% test_config('--user_tag', '').

:- export(get_test_config/2).
get_test_config(A, B) :-
    test_config(A, B), !.

:- export(set_test_config/2).
set_test_config(K,_) :-
    retract_fact(test_config(K, _)),
    fail.
set_test_config(K,V) :-
    assertz_fact(test_config(K,V)).

:- export(get_edit_mode/1).
% TODO: this needs to be unified
get_edit_mode(EM) :-
    ( get_file_config(edition_mode(EM)) ; get_test_config('--group', EM) ), !.

:- export(file_config/1).
:- data file_config/1.
:- export(get_file_config/1).
get_file_config(X) :-
    file_config(X), !.
:- export(read_config_file/1).
read_config_file(File) :-
    file_to_terms(File,Ts),
    ( % failure-driven loop
      member(T, Ts),
        assertz_fact(file_config(T)),
        fail
    ; true ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, "Test location predicates").

:- export(test_dir/4).
:- export(bndls_dir/1).

:- include(test_dirs).

:- use_module(engine(internals), [ciao_root/1]).
:- use_module(library(pathnames)).

bndls_dir(BundlesDir) :-
    ciao_root(D),
    path_concat(D,'bndls',BundlesDir).

:- export(test_toplevel/1).
:- data test_toplevel/1.

:- export(test_dir/2).
:- data test_dir/2.
