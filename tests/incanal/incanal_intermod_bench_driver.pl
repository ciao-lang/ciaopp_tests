:- module(incanal_intermod_bench_driver, [main/1], [assertions, regtypes, datafacts, fsyntax]).

:- doc(title, "Benchmark driver for incremental modular experiments").

:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "

This is a driver for performing tests of intermod driver.

The test performed consists of analyzing different states of the sets
of modules of a give directory. This is done by adding a number of
random clauses each iteration of the test and performing the analysis
afterwards.

Results of the analysis are left in the directory from which the test
was runned.

@bf{Important!:} Each time a benchmark is going to be rerunned the
previous analysis directory has to be removed.

This module is thought to be used as an executable:

@begin{verbatim}
 ./exec <bench_id> <domain> [Opts]
@end{verbatim}

The seed is used to be able to repeat the same 'random' sequence in several tests.

For example:

@begin{verbatim}
./exec qsort gr --edit_type add --n_edits 5 incremental modular --rand 1
@end{verbatim}

will perform a test of adding 5 random clauses in the files of
benchmark qsort with an incremental modular analysis.

The benchmarks that can be currently performed are present in @tt{test_dirs.pl}.

@subsection{Options available}
@includedef{config_opt/1}

@section{Template for bundle configuration}
@includecode{bundle_config_template.pl}

@section{Template for experiment configuration}
@includecode{experiment_config_template.pl}
").

:- use_module(library(lists), [member/2]).
:- use_module(library(messages), [show_message/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system)).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(engine(runtime_control), [set_prolog_flag/2]).
:- use_module(engine(stream_basic), [absolute_file_name/7]).
:- use_module(library(streams), [fixed_absolute_file_name/3]).
:- use_module(library(errhandle), [error_protect/2]).

:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp_tests(incanal/incanal_intermod_test)).
:- use_module(ciaopp_tests(incanal/config_db)).
:- use_module(ciaopp(plai/incanal/incanal_persistent_db), [set_inc_persistent/1]).
:- use_module(ciaopp(frontend_driver), [module/1, ensure_libcache_loaded/0]).
:- use_module(ciaopp(analyze_driver)).
:- use_module(ciaopp(raw_printer), [show_analysis/0]).
:- use_module(ciaopp(plai/domains), []).
:- multifile aidomain/1. % TODO: how do I import this?

main(X) :-
    error_protect(main_(X), fail).

main_([Bench, Domain|Opts]) :- !,
    ( aidomain(Domain) ->
        test(Bench, Domain, Opts)
    ;
        show_message(simple, "ERROR: not a valid domain: ~w", [Domain]),
        halt(1)
    ).
main_(_) :-
    show_message(simple, "Usage: ./exec <bench> <domain> Opts\n",[]),
    halt(1).

edit_num(add, 1).
edit_num(del, 0).

set_common_flags :-  % monolithic by default
    % prolog flags
    set_prolog_flag(discontiguous_warnings,off),
    set_prolog_flag(multi_arity_warnings,off),
    set_prolog_flag(write_strings, on),
    set_prolog_flag(rtchecks_callloc, literal),
    % ciaopp flags
    set_pp_flag(fixpoint, dd),
    set_pp_flag(intermod, on), % modular driver by default
    set_pp_flag(incremental, off),
    set_pp_flag(remove_useless_abs_info, on),
    set_pp_flag(preload_lib_sources, on),
    % for checking the results, it will be set to off if incanal
    set_pp_flag(ext_policy, registry), % Other policies do not make sense
    set_pp_flag(entry_policy, top_level),
    set_pp_flag(mnu_modules_to_analyze, all),
    set_pp_flag(module_loading, all), % monolithic by default
    set_pp_flag(punit_boundary, bundle),
    set_pp_flag(widen, on),
    set_pp_flag(success_policy, under_all),
    set_pp_flag(clique_widen, amgu),
    set_pp_flag(clique_widen_type, panic_1),
    set_pp_flag(clique_widen_ub, 10),
    set_pp_flag(clique_widen_lb, 10),
%    set_pp_flag(type_precision, defined),
    current_pp_flag(pplog, Logs),
    set_pp_flag(pplog, [p_abs,incremental_high|Logs]).
%    set_pp_flag(types,deftypes).
%        set_pp_flag(timestamp_trace, on).

test(BenchId, Domain, ConfigOpts) :-
    set_common_flags,
    make_config(ConfigOpts),
    working_directory(_, ~bundle_path(ciaopp_tests, 'tests/incanal/bench')),
    ( member(basic, ConfigOpts) ->
        ( test_dir(Top, BenchId, Bench0, _Type) ->
            fixed_absolute_file_name(Bench0,'.',BenchDir),
            ensure_libcache_loaded, !,
            basic_analyze(Top, BenchDir, Domain)
        ;
            show_message(error, "Bench not found: ~w~n", [BenchId]),
            halt(1)
        )
    ;
        ensure_libcache_loaded, !,
        test(BenchId, ['--domain', Domain])
    ).

make_config([]).
make_config([C|Cfgs]) :- !,
    process_config(C,Cfgs,CCfgs),
    make_config(CCfgs).

process_config(C,[File|Cs],Cs) :-
    atom_concat('--config-',_,C), !,
    working_directory(W,W),
    absolute_file_name(File,'','.pl',W,AbsFile,_,_),
    ( \+ file_exists(AbsFile) ->
        show_message(error, "Config file not found: ~w~n", [AbsFile]),
        halt(1)
    ;
        show_message(simple, "Detected config file: ~w~n", [AbsFile])
    ),
    set_test_config(C,AbsFile),
    read_config_file(AbsFile).
process_config(Show,Cs,Cs) :-
    atom_concat('--show-',_,Show), !,
    set_test_config(Show, _).
process_config(O, [A|Cs], Cs) :-
    num_opt(O), !,
    atom_number(A, N),
    set_test_config(O, N).
process_config('--debug-steps',Cs,Cs) :- !,
    set_test_config('--debug-steps', 0).
process_config('--debug-mod-reg',Cs,Cs) :- !,
    current_pp_flag(pplog, Logs),
    set_pp_flag(pplog, [intermod_reg|Logs]).
process_config('--debug-mod-ana',Cs,Cs) :- !,
    current_pp_flag(pplog, Logs),
    set_pp_flag(pplog, [intermod_dump|Logs]).
process_config('--user_tag',[C|Cs],Cs) :- !,
    set_test_config('--user_tag',C).
process_config('--edit_type',[C|Cs],Cs) :- !,
    set_test_config('--edit_type',C).
process_config(C,Cs,Cs) :-
    config(C).

num_opt('--start').
num_opt('--steps').
num_opt('--rand').
num_opt('--n_edits').

:- pred config/1 : config_opt.
config(monolithic_driver) :- !, % warning!!! this only works if the program is
    % self-contained
    set_pp_flag(intermod, off).
config(incremental) :- !,
    set_pp_flag(remove_useless_abs_info, off),
    set_pp_flag(incremental, on).
config(modular) :- !,
    set_pp_flag(module_loading, one),
    set_pp_flag(type_precision, defined). % to reach fixpoint
config(bottom_up) :- !,
    set_pp_flag(del_strategy, bottom_up).
config(top_down) :- !,
    set_pp_flag(del_strategy, top_down).
config(bottom_up_cls) :- !,
    set_pp_flag(del_strategy, bottom_up_cls).
config(monolithic) :- !,
    set_pp_flag(module_loading, all),
    set_fact(monolithic).
config(plai) :- !,
    set_pp_flag(incremental, off),
    set_pp_flag(fixpoint, plai).
config(di) :- !,
    set_pp_flag(incremental, off),
    set_pp_flag(fixpoint, di).
config(dd) :- !,
    set_pp_flag(fixpoint, dd).
config(basic) :- !,
    set_pp_flag(incremental, off),
    set_pp_flag(intermod, off).
config(SuccessPolicy) :-
    set_pp_flag(success_policy, SuccessPolicy), !.
config(trace) :- !,
    set_pp_flag(trace_fixp, trace).
config(entry) :- !,
    set_pp_flag(entry_points_auto, none).
config(fact_info) :- !,
    set_pp_flag(fact_info, on).
config(assertions) :- !,
    set_pp_flag(old_trusts, off),
    set_pp_flag(use_check_as_trust, on).
config(verbose) :- !,
    set_pp_flag(verbosity, very_high).
config(X) :-
    show_message(note, "Config not available for: ~w", [X]).

:- regtype config_opt/1.
:- doc(config_opt/1, "Options to config the test.").
% TODO: document options
config_opt(incremental).
config_opt(modular).
config_opt(bottom_up).
config_opt(bottom_up_cls).
config_opt(top_down).
config_opt(monolithic).
config_opt(plai).
config_opt(dd).
config_opt(di).
config_opt(no_pers).
config_opt(basic).
config_opt(trace).
config_opt(fact_info).
config_opt(under_all).
config_opt(over_all).
config_opt(assertions).
config_opt(verbose).
config_opt(debug_cls).
config_opt('--show-gat').
config_opt('--show-lat').
config_opt('--show-cls').
config_opt('--show-raw-output').
config_opt('--config-exp').
config_opt('--config-bndl').
config_opt('--debug-steps').
config_opt('--start').
config_opt('--steps').
config_opt('--seq_sz').
config_opt('--user_tag').
config_opt('--edit_type').

basic_analyze(Top, Dir, Domain) :-
    module(~path_concat(Dir, Top)),
    analyze(Domain),
    show_analysis.
