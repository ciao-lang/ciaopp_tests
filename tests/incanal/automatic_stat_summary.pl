:- module(automatic_stat_summary, [main/0], [assertions, hiord, datafacts]).

:- use_module(library(lists), [member/2, length/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(format), [format/2]).

:- use_module(ciaopp_tests(incanal/summarize_stat),
    [summarize_dir/2, all_tmp_stat/2]).

:- doc(module, " 

This module obtains and reports general statistics about benchmark tests of
    incremental modular analysis.

To run this module, first the benchmarks have to be
execute. `run_all.sh` will generate the statistics of the analyses.
").


:- data bench_property/4.
% bench_property(feat, bench_id, opts, value).

:- meta_predicate property_to_check(?,?,?,pred(2), ?).
property_to_check([Op, Modular, ''], [Op, Modular, incremental|Del], ana_time, >, 'Incremental is faster'(Op,Modular,Del)) :-
    ( Op = add, Del = []
    ;
      Op = del,
        (Del = [top_down] ; Del = [bottom_up])),
    ( Modular = monolithic ; Modular = modular ).

%property_to_check([add, modular, ''], [add, modular, incremental], ana_time, >).
%property_to_check([add, monolithic, ''], [add, monolithic, incremental], ana_time, >).
%property_to_check([del, monolithic, ''], [del, monolithic, incremental, bottom_up], ana_time, >).
%property_to_check([del, modular, incremental, top_down], [del, modular, incremental, bottom_up], ana_time, >).

main :-
    retractall_fact(bench_property(_, _, _, _)),
    ( % failure-driven loop
      bench(BId, BOpts, BDir), % bench generator
        process_bench(BId, BOpts, BDir),
        % fill bench_property(s) for this bench and opts
        fail
    ; true),
    get_conclusions.

get_conclusions :-
    ( % failure-driven loop
      property_to_check(Opts1, Opts2, Feat, Cmp, Disp),
        check_property(Opts1, Opts2, Feat, Cmp, NHolds, NTotal, Left),
        display(Disp),
        format('in ~w/~w benchmarks.\n', [NHolds, NTotal]),
        ( Left = [] -> true
        ;
            format('Exceptions:\n', []),
            ( % failure-driven loop
                member(X, Left), display(X), nl, fail ; true),
                fail
        ),
        fail
    ; true).


:- data total/0.
:- data holds/0.
:- data left/1.

:- meta_predicate check_property(+, +, +, pred(2), -, -, -).
check_property(Opts1, Opts2, Feat, Cmp, NHolds, NTotal, Left) :-
    retractall_fact(total),
    retractall_fact(holds),
    retractall_fact(left(_)),
    ( % failure-driven loop
      bench_property(Feat, BenchId, OptsB1, Value1),
      Opts1 = OptsB1,
        %
        bench_property(Feat, BenchId, OptsB2, Value2),
        Opts2 = OptsB2,
        assertz_fact(total),
        ( Cmp(Value1, Value2) ->
            assertz_fact(holds)
        ;
            assertz_fact(left(BenchId))
        ),
        fail
    ; true ),
    findall(_, total, L),
    length(L, NTotal),
    findall(_, holds, L2),
    length(L2, NHolds),
    findall(X, left(X), Left).
    
process_bench(BId, BOpts, BDir) :-
    summarize_dir(BDir, dont_display),
    ( % failure-driven loop
       all_tmp_stat(Stat, Value),
       assertz_fact(bench_property(Stat, BId, BOpts, Value)),
       %display(bench_property(Stat, BId, BOpts, Value)), nl,
       fail
    ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INCREMENTAL BENCH DRIVER SPECIFIC PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(pathnames), [path_concat/3]).

bench_id(aiakl).
bench_id(ann).
bench_id(bid).
bench_id(boyer).
bench_id(hanoi).
bench_id(mmutr).
bench_id(qsort).


bench_opt_value(edition_action, X) :-
    member(X, [add, del]).
bench_opt_value(modularity, X) :-
    member(X, [monolithic, modular]).
bench_opt_value(incrementality, X) :-
    member(X, ['', incremental]).
bench_opt_value(del_strategy, X) :-
    member(X, [top_down, bottom_up]).
% TODO:
% This could be obtained from preprocess_flags but valid_flag_values does not help
%
% bench_opt_value(Opt, X) :-
%       bench_opt(Opt),
%       valid_flag_value(Opt, X).
%
% bench_opt(incremental).
% bench_opt(del_strategy).

:- export(bench/3).
bench(BId, BOpts, BDir) :- % This is not general
    EditF = add,
    bench_id(BId),
    bench_opt_value(modularity, ModF),
    bench_opt_value(incrementality, IncF),
    build_add_results_dir(BId, EditF, ModF, IncF, BDir),
    BOpts = [EditF, ModF, IncF].
bench(BId, BOpts, BDir) :- % This is not general
    EditF = del,
    bench_id(BId),
    bench_opt_value(modularity, ModF),
    bench_opt_value(incrementality, IncF),
    ( IncF = incremental ->
        bench_opt_value(del_strategy, DelF),
        build_del_results_dir(BId, EditF, ModF, IncF, DelF, BDir),
        LOpts = [DelF]
    ;
        build_add_results_dir(BId, EditF, ModF, IncF, BDir),
        LOpts = []
    ),
    BOpts = [EditF, ModF, IncF|LOpts].
build_add_results_dir(BId, EditF, ModF, IncF, BDir) :-
    atom_concat(BId, '_', A1),
    atom_concat(A1, EditF, A2),
    atom_concat(ModF, '_', B1),
    atom_concat(B1, IncF, B2),
    atom_concat(B2, '_stats', B3),
    path_concat(A2, B3, BDir).
build_del_results_dir(BId, EditF, ModF, IncF, DelF, BDir) :-
    atom_concat(BId, '_', A1),
    atom_concat(A1, EditF, A2),
    atom_concat(ModF, '_', B1),
    atom_concat(B1, IncF, B2),
    atom_concat(B2, '_', B3),
    atom_concat(B3, DelF, B4),
    atom_concat(B4, '_stats', B5),
    path_concat(A2, B5, BDir).

%extract here encoding of opts for the path
%encode_opts(Opts) :-
%   nonincremental -> ''

% TODO:
% All these atom_concat would be simpler if we had something like
% sprintf in C for atoms
