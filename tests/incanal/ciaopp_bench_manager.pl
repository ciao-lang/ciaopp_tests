:- module(ciaopp_bench_manager, [], [assertions, regtypes]).

:- doc(title, "CiaoPP incremental modular benchmark manager").

:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "This module is intended to be a Ciao interface for the
    scripts for running benchmaks of (modular) (incremental) analysis.

Before running any comman, the bench drivers have to be compiled:
@begin{verbatim}
% load this module
?- compile_all. % generate executables
yes

@end{verbatim}

@section{Usage}

Running and generating graphs of several benchmarks
@begin{verbatim}
?- run_benchmark(aiakl, add, []).
%
% Benchmark text
yes
?- run_benchmark(aiakl, del, []).
% ...
yes
?- generate_results_summary(\"aiakl*\"). 
% generates a graph with aiakl add and del tests
% This will open in your default pdf viewer the generated graph
yes
?- run_benchmark(qsort, add, []).
% ...
yes
?- generate_results_summary(\"*-add-\"). 
% generates a graph for all the tests of adding sequences
% This will open in your default pdf viewer the generated graph
yes

?- show_performed_tests_directory.
aiakl-add-not_rand-1-shfr-dd
aiakl-del-not_rand-1-shfr-dd
graphs
qsort-add-not_rand-1-shfr-dd
..
.

@end{verbatim}
").

:- use_module(engine(io_basic)).
:- use_module(library(process), [process_call/3]).

:- use_module(ciaopp_tests(incanal/incanal_intermod_test), [all_tests_results_dir/1]).

:- export(run_benchmark/3).
:- pred run_benchmark(Bench, Edition, Opts) : (atm(Bench), edition(Edition), list(Opts))
    #"Runs all configurations of incremental and modular analysis
      for benchmark @var{Bench} and @var{Edition} for the edition
      simulation sequence. Note that if there were previous
      results of a benchmark with the same configuration, those
      will be overwritten".
run_benchmark(Bench, Edition, _Opts) :-
    process_call('./run_configs.sh', [Bench,Edition], []).

:- regtype edition/1.
edition(add).
edition(del).

:- export(compile_all/0).
:- doc(compile_all/0, "Compiles all executables to perform tests.").
compile_all :-
    process_call('./compile.sh', [], []).

check_benchmark_correctness. % --> compare analysis

summarize_bench_stats(_). % --> prolog_to_table usages

:- export(generate_results_summary/1).
:- pred generate_results_summary(Filter) : string
    #"It generates:
      @begin{itemize}
      @item A graph with all the tests that were runned (for example with @pred{run_benchmark/3}) that meet the filter.
      @end{itemize}
      ".
generate_results_summary(Filter) :-
    all_tests_results_dir(Dir),
    atom_codes(AF, Filter),
    display('Detailed graphs are written in Dir in subdirectory with the current timestamp.'), nl,
    process_call('./generate_result_summary.sh', [Dir, AF], []).

:- export(show_performed_tests_directory/0).
:- doc(show_performed_tests_directory/0, "Shows a list of the tests that were
    performed by the user.").
show_performed_tests_directory :-
    all_tests_results_dir(Dir),
    ( % failure-driven loop
      directory_dir(Dir, _, F),
        display(F), nl,
        fail
    ;
        true
    ).

% ---------------------------------------------------------------------------

:- use_module(library(lists), [member/2]).
:- use_module(library(system), [directory_files/2, file_property/2]).
:- use_module(library(pathnames), [path_concat/3]).

:- export(directory_dir/3).
directory_dir(Dir, Base, F1) :-
    directory_files(Dir, Fs),
    member(Base, Fs),
    path_concat(Dir, Base, F1),
    file_property(F1, type(directory)).

% ---------------------------------------------------------------------------

% Predicates to access the results (paths)
% Predicates to access the generated visualizations

% Predicates to get all the options?
