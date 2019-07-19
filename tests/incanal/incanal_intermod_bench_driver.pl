:- module(incanal_intermod_bench_driver, [main/1], [assertions, regtypes, datafacts]).

:- use_module(library(lists), [member/2]).
:- use_module(library(messages), [show_message/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [working_directory/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(engine(runtime_control), [set_prolog_flag/2]).
:- use_module(library(streams), [fixed_absolute_file_name/3]).
:- use_module(library(errhandle), [error_protect/2]).

:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2]).
:- use_module(ciaopp_tests(incanal/incanal_intermod_test),
	[test/2, set_test_config/2, get_test_config/2]).
:- use_module(ciaopp(plai/incanal/incanal_persistent_db), [set_inc_persistent/1]).
:- use_module(ciaopp(frontend_driver), [module/1, ensure_lib_sources_loaded/0]).
:- use_module(ciaopp(analyze_driver)).
:- use_module(ciaopp(raw_printer), [show_analysis/0]).

:- include(test_dirs).

:- doc(title, "Benchmark driver for incremental modular experiments").

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
 ./exec <bench_name> <#changes/iteration> <edition_type> <domain> Opts
@end{verbatim}

The seed is used to be able to repeat the same 'random' sequence in several tests.

For example:

@begin{verbatim}
./exec qsort 5 add gr incremental modular under_all --rand 1
@end{verbatim}

will perform a test of adding 5 random clauses in the files of
benchmark qsort with an incremental modular analysis.

The benchmarks that can be currently performed are present in @tt{test_dirs.pl}.
.").

:- export(monolithic/0).
:- doc(monolithic/0, "Flag to store whether the analysis is monolithic or not.").
:- data monolithic/0.

main(X) :-
        error_protect(main_(X), fail).

main_([Bench, EditionType, N, Domain|Opts]) :- 
	bundle_path(ciaopp_tests, 'tests/incanal/bench', Path),
	working_directory(_, Path),
	atom_number(N, N1),
	edit_num(EditionType, EditT), !,
  set_pp_flag(preload_lib_sources, on),
	ensure_lib_sources_loaded,
	test(Bench, EditT, Opts, N1, Domain).
main_(_) :-
	show_message(simple, "Usage: ./exec <bench> <add/del> <#changes/iteration> <domain> Opts\n",[]).

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
        set_pp_flag(ext_policy, registry), % Other policies do not make sense
        set_pp_flag(entry_policy, top_level),
        set_pp_flag(mnu_modules_to_analyze, all),
        set_pp_flag(module_loading, all), % monolithic by default
        set_pp_flag(punit_boundary, bundle),
        set_pp_flag(widen, on),
        set_pp_flag(success_policy, under_all),
        set_pp_flag(preload_lib_sources, on),
        set_pp_flag(clique_widen, amgu),
        set_pp_flag(clique_widen_type, panic_1),
        set_pp_flag(clique_widen_ub, 10),
        set_pp_flag(clique_widen_lb, 10).
%        set_pp_flag(timestamp_trace, on).
%  set_pp_flag(fact_info, on). % Necessary for incanal trust success

test(Bench, EditT, ConfigOpts, NCh, Domain) :-
	retractall_fact(test_config_(_,_)),
	set_common_flags,
	make_config(ConfigOpts),
  ( test_dir(Top, Bench, Bench0, Type) ->
      fixed_absolute_file_name(Bench0,'.',BenchDir),
      ( member(basic, ConfigOpts) ->
          basic_analyze(Top, BenchDir, Domain)
      ;
          TOpts = ['n_edits', NCh, 'edit_type', EditT, 'domain', Domain],
          test(bench(Bench, Top, BenchDir,Type), TOpts)
      )
  ;
        show_message(error, "Bench not found: ~w", [Bench])
  ).

make_config([]).
make_config(['--start', A|Cfs]) :- !,
	atom_number(A, N),
	set_test_config('--start', N),
	make_config(Cfs).
make_config(['--steps', A|Cfs]) :- !,
	atom_number(A, N),
	set_test_config('--steps', N),
	make_config(Cfs).
make_config(['--rand', A|Cfs]) :- !,
	atom_number(A, Seed),
	set_test_config('--rand', Seed),
	make_config(Cfs).
make_config([X, A|Cfs]) :-
	get_test_config(X, _), !,
	set_test_config(X, A),
	make_config(Cfs).
make_config([Cf|Cfs]) :-
	config(Cf),
	make_config(Cfs).

:- data test_config_/2.

test_config(X, Conf) :-
	test_config_(X, Conf), !.
test_config(_, 'no').

config(incremental) :- !,
	set_pp_flag(incremental, on).
config(modular) :- !,
	set_pp_flag(module_loading, one).
	%set_pp_flag(type_precision, defined). % to reach fixpoint
config(bottom_up) :- !,
	set_pp_flag(del_strategy, bottom_up).
config(top_down) :- !,
	set_pp_flag(del_strategy, top_down).
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
        set_pp_flag(entry_point, entry).
config(fact_info) :- !,
        set_pp_flag(fact_info, on).
config(assertions) :- !,
        set_pp_flag(old_trusts, off),
        set_pp_flag(use_check_as_trust, on).
config(X) :-
	show_message(note, "Config not available for: ~w", [X]).

:- regtype config_opt/1.
:- doc(config_opt/1, "Options to config the test.").
config_opt(incremental).
config_opt(modular).
config_opt(bottom_up).
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

basic_analyze(Top, Dir, Domain) :-
	path_concat(Dir, Top, ModPath),
	module(ModPath),
	analyze(Domain),
	show_analysis.
