:- module(incanal_intermod_test, [test/2], [assertions, datafacts]).

:- doc(title, "Incremental analysis tester").

:- doc(module, "This module contains the functionality to perform test of analysis.

This is a low-level interface, for analyzing consistently with ciaopp
flags, use the incanal_intermod_bench_driver module.").

:- use_module(library(sort), [sort/2]).
:- use_module(library(lists), [member/2, reverse/2,append/3]).
:- use_module(library(system), [directory_files/2, make_directory/1, file_exists/1]).
:- use_module(library(pathnames), [path_concat/3, path_splitext/3, path_split/3, path_basename/2]).
:- use_module(library(fastrw), [fast_write/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3,reverse_bundle_path/3]).
:- use_module(library(arithpreds), [ceiling/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(random), [srandom/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(engine(runtime_control), [garbage_collect/0]).
:- use_module(engine(stream_basic)).
:- use_module(library(messages), [error_message/2,show_message/3]).
:- use_module(engine(messages_basic)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(ciaopp(analyze_driver), [analyze/2]).
:- use_module(ciaopp(frontend_driver), [module/1]).
:- use_module(ciaopp(plai/intermod), [intermod_analyze/3]).
:- use_module(ciaopp(p_unit/p_dump), [dump_dir/1, dump/2]).
:- use_module(ciaopp(p_unit/p_abs), [registry/3, registry_headers/2, write_registry_file/3]).
:- use_module(ciaopp(raw_printer)).

:- use_module(ciaopp(analysis_stats), [clean_stat_steps/0, dump_steps/1]).
:- use_module(ciaopp_tests(incanal/naive_reader)).
:- use_module(ciaopp_tests(incanal/edition_sequence_generator)).
:- use_module(ciaopp_tests(incanal/module_writer)).

:- use_module(ciaopp_tests(incanal/git_wrapper)).

:- export(all_tests_results_dir/1).
all_tests_results_dir(Dir) :-
    bundle_path(ciaopp_tests, 'tests/incanal/test_results', Dir),
    decide_make_dir(Dir).

:- data test_config/2.
% default opts
test_config('--group', clause). % clause ; pred % TODO: pred not implemented yet
test_config('--one_mod', yes). % yes ; no
test_config('--seq_sz', abs). % abs ; rel
%test_config('rand', yes). % yes ; no
test_config('edit_type', add). % add ; del
test_config('n_edits', 1). % int
test_config('domain', shfr).
test_config('--user_tag', '').

:- export(get_test_config/2).
get_test_config(A, B) :-
    test_config(A, B), !.

code_test_all_config_dir(Dir, Id, CDir) :-
    ( get_test_config('edit_type', 1) ->
        ET = add,
        DE = []
    ;
        current_pp_flag(del_strategy, Del),
        ET = del,
        DE = ['-', Del]
    ),
    ( current_pp_flag(incremental, on) -> Inc = inc ; Inc = noninc),
    get_test_config('n_edits', NE),
    number_atom(NE, NEAtm),
    get_test_config('domain', AI),
    get_test_config('--user_tag', UId0),
    ( UId0 = '' -> UIdL = [] ; UIdL = ['-', UId0]),
    ( get_test_config('--rand', Seed) ->
      number_atom(Seed, SeedAtm),
        Rand = [rand, '-', SeedAtm|X]
    ;   Rand = [not_rand|X]
    ),
    current_pp_flag(fixpoint, FP),
    ( current_pp_flag(module_loading, all) -> Mod = mon ; Mod = mod ),
    X = ['-',NEAtm,'-',AI,'-',FP|UIdL],
    SubDir = [Mod,'-',Inc|DE],
    atom_concat([Id,'-',ET,'-'|Rand], TC),
    path_concat(Dir, TC, D1),
    decide_make_dir(D1),
    atom_concat(SubDir, SD),
    path_concat(D1, SD, CDir),
    decide_make_dir(CDir).

number_atom(N, A) :-
    number_codes(N, C),
    atom_codes(A, C).

:- export(set_test_config/2).
set_test_config(K,_) :-
    retract_fact(test_config(K, _)),
    fail.
set_test_config(K,V) :-
    assertz_fact(test_config(K,V)).

set_configs([]).
set_configs([C|Cs]) :-
    retract_fact(test_config(C, _)), !,
    Cs = [Value|Cs2],
    assertz_fact(test_config(C, Value)),
    set_configs(Cs2).
set_configs([C|Cs]) :-
    error_message("Option not available: ~w", [C]),
    set_configs(Cs).

:- pred get_results_dir(Id, TDir) #"@var{TDir} is the location of results
    in this bundle. A directory is created.".
get_results_dir(Id, TDir) :-
    all_tests_results_dir(Dir),
    code_test_all_config_dir(Dir, Id, TDir).

:- export(gat_test_results_dir/2).
gat_test_results_dir(RDir, GTDir) :-
    path_concat(RDir, 'detailed_step_results', GTDir).

stat_test_results_dir(RDir, STDir) :-
    path_concat(RDir, 'stats', STDir).

:- data test_toplevel/1.
:- data test_dir/1.

code_db_from_dir(D) :-
    clean_reader,
    directory_files(D, Fs0),
    sort(Fs0, Fs1),
    reverse(Fs1, Fs),
    ( member(F, Fs),  % failure driven loop: read the code in all modules
        atom_concat(ModName, '.pl', F),
        path_concat(D, F, ModPath),
        read_module(ModName, ModPath),
        fail
    ; true ).

:- pred test(Bench, Opts) #"
Runs a test of analysis. The configuration has is expressed as follows:

@begin{itemize}
@item @var{TestId}: Identifier of the test, it must be specified in
the test_dirs.pl file as the name of the directory in the bench
directory that contains the test to be performed.
@item @var{TestTopLevel}: Main module of the (modular) program.
@comment{@item @var{NEdits}: Number of editions each step of the test (number
of clauses to be added or deleted).}
@item @var{EditType}: States whether the test is about adding or removing clauses
@item @var{TestDir}: Directory of the test.
@item @var{AbsInt}: Abstract domain that will be used for testing
@end{itemize}
".
test(bench(TestId, TestTopLevel, SrcDir,DirType), Opts) :-
    set_configs(Opts),
    set_fact(test_iteration(0)),
    set_fact(test_toplevel(TestTopLevel)),
    get_analysis_dir(TestId,AnaDir), % directory where analysis is performed
    get_results_dir(TestId,ResDir), % directory where results are stored
    generate_simulation_sequence(DirType,SrcDir,Seq),
    set_fact(test_dir(TestId)),
    init_results_directories(ResDir), % move before generating sim seq
    write_sequence_file(ResDir,Seq),
    ( get_test_config('--debug', _) ->
        write_dir_state_sequence(Seq, DirType,ResDir)
    ;   true
    ),
    clean_ana_dir(AnaDir),
    path_concat(AnaDir,TestTopLevel,TLPath),
    perform_seq_analysis(Seq,DirType,TLPath,AnaDir).

get_analysis_dir(TestId,AnaDir) :- % TODO: hardwired
    atom_codes(TestId,TestIdL),
    append("lpdoc", _, TestIdL), !,
    bundle_path(lpdoc_asr_inc, '.', AnaDir).
get_analysis_dir(TestId,AnaDir) :-
    get_results_dir(TestId,AnaDir).

clean_ana_dir(AnaDir) :-
    path_concat(AnaDir,'src/version_auto.pl',VPath),  % TODO: !!
    ( file_exists(VPath) ->
        path_concat(AnaDir,'src/version_auto_fixed.pl',VPath2),
        catch(process_call(path(mv), [VPath, VPath2], []), _, true),
        clean_tree(AnaDir),
        catch(process_call(path(mv), [VPath2, VPath], []), _, true)
    ; true),
    process_call(path(find), [AnaDir, '-type', 'f', '-name', '*.reg', '-delete'], []),
    process_call(path(find), [AnaDir, '-type', 'f', '-name', '*.dump', '-delete'], []),
    process_call(path(find), [AnaDir, '-type', 'f', '-name', '*.dump_inc', '-delete'], []).
    
generate_simulation_sequence(manual, SrcDir, Seq) :-
    code_db_from_dir(SrcDir),
    get_code_summary(Sum, NCls),
    get_test_config('n_edits', NEdits),
    TotalNSteps is NCls / NEdits,
    ceiling(TotalNSteps, IntSteps),
    generate_sim_seq_(Sum, NCls, IntSteps, NEdits, Seq2, EditType),
    get_seq_limits(IntSteps, StepStart, NSteps),
    limit_sequence(Seq2, StepStart, NSteps, Seq),
    complementary_edit_type_(EditType, CET),
    fill_seq(Seq, CET).
generate_simulation_sequence(git, SrcDir, Seq) :-
    load_git_repo(SrcDir,incanal_git),
    % currenlty for git repos, only the order of commit is available
    findall(Commit, git_log(incanal_git,_,Commit), Seq),
    ( reverse_bundle_path(SrcDir,_,_) ->
      clean_ana_dir(SrcDir)
    ; true
    ).
generate_simulation_sequence(states,SrcDir,Seq) :-
    directory_files(SrcDir,Seq0),
    sort(Seq0,Seq1),
    findall(X, filter_directories(Seq1,SrcDir,X), Seq).

filter_directories(Seq, SrcDir, X) :-
    member(D,Seq),
    atom_codes(D,LD),
    ( append("state_", _, LD) -> true ; fail ),
    path_concat(SrcDir,D,X).

% edit not_rand mod_by_mod
generate_sim_seq_(Sum, NCls, Steps, NEdits, Seq, 1) :-
    \+ get_test_config('--rand', _),
    get_test_config('--one_mod', OneMod), !,
    ( OneMod = yes ->
        generate_edit_sequence_mods(Sum, NCls, Steps, NEdits, 1, Seq1)
    ;
        generate_edit_sequence_uniform_mods(Sum, NCls, Steps, NEdits, 1, Seq1)
    ),
    get_test_config('edit_type', EditT),
    ( EditT = 0 -> % for deleting
      reverse(Seq1, Seq)
    ; Seq = Seq1).
% edit rand
generate_sim_seq_(Sum, NCls, Steps, NEdits, Seq, EditT) :-
    get_test_config('--rand', Seed),
    srandom(Seed),
    get_test_config('edit_type', EditT),
    generate_random_edit_sequence(Sum, NCls, Steps, NEdits, EditT, Seq).

get_seq_limits(_, Start, Steps) :-
    get_test_config('--seq_sz', abs), !,
    ( get_test_config('--start', Start) -> true
    ; Start = no ),
    ( get_test_config('--steps', Steps) -> true
    ; Steps = no
    ).
get_seq_limits(TotalSteps, Start, Steps) :-
    ( get_test_config('--start', StartRatio) ->
        FStart is (StartRatio/100) * TotalSteps,
        ceiling(FStart, Start)
    ; Start = no ),
    ( get_test_config('--steps', StepsRatio) ->
        FSteps is (StepsRatio/100) * TotalSteps,
        ceiling(FSteps, Steps)
    ; Steps = no
    ).

init_results_directories(ResDir) :-
    gat_test_results_dir(ResDir, GDir),
    catch(make_directory(GDir), E, dir_exists_handler(GDir)),
    stat_test_results_dir(ResDir, SDir),
    catch(make_directory(SDir), E, dir_exists_handler(SDir)).

dir_exists_handler(Dir) :-
    path_split(Dir, Base, _),
    error_message("Directory exists, remove it to redo the test: \n~w",[Base]),
  abort.

complementary_edit_type_(1,0).
complementary_edit_type_(0,1).

fill_seq([], _).
fill_seq([St|Seq], EditType) :-
    fill_dir(St, EditType),
    fill_seq(Seq, EditType).

fill_dir([], _).
fill_dir([present(_, ClList)|IniState], EditType) :-
    fill_list(ClList, EditType),
    fill_dir(IniState, EditType).

fill_list([], _).
fill_list([X|Xs], EditType) :-
    X = EditType, !,
    fill_list(Xs, EditType).
fill_list([_|Xs], EditType) :-
    fill_list(Xs, EditType).

perform_seq_analysis([], _, _, _).
perform_seq_analysis([St|Seq], DirType, TopLevel, Dir) :-
    % '$metachoice'(Chpt),
    % display(user_error, chpt_seq_analysis(Chpt)), nl(user_error),
    %
    perform_seq_analysis1(St, DirType, TopLevel, Dir), !,
    % TODO: This cut removes choicepoints (the choicepoints were making
    % retracting facts very slow, because it had to check them)
    perform_seq_analysis(Seq, DirType, TopLevel, Dir).

:- op(970, fx, (nochpt)).

:- use_module(engine(hiord_rt), [call/1]).

% TODO: move??
:- meta_predicate nochpt(goal).
:- export(nochpt/1).
nochpt(G) :-
    '$metachoice'(Chpt0),
    call(G),
    '$metachoice'(Chpt),
    ( Chpt == Chpt0 -> true
    ; error_message("WARNING: expected NOCHPT!! ~q", [G])
    ).

perform_seq_analysis1(St, DirType, TopLevel, Dir) :-
    write_dir_state(St, DirType, Dir,git_checkout_copy),
    test_config(domain, AbsInt), !,
    clean_stat_steps,
    inc_test_iteration(N),
    display_list(['\n---------- Edition iteration ', N, ' ----------\n\n']),
    garbage_collect,
    set_dir_dump_lat,
    ( ( current_pp_flag(intermod, off) ->
        module(TopLevel),
        analyze(AbsInt, _Stats)
    ;  intermod_analyze(AbsInt, TopLevel, _Stats)
    )
      ->
        ( get_test_config('--show-gat',_) -> show_global_answer_table(AbsInt) ; true ),
        ( get_test_config('--show-lat',_) -> show_analysis ; true ),
        ( get_test_config('--show-cls',_) -> show_trans_clauses ; true ),
        ( get_test_config('--show-raw-output',_) -> raw_output(user) ; true )
    ; error_message("ANALYSIS FAILED", [])
    ),
    edition_stat_file(StatFile),
    dump_steps(StatFile),
    dump_gat.

write_sequence_file(DstDir, Seq) :-
    path_concat(DstDir, 'seq.bin', F),
    open(F, write, S),
    fast_write(S, Seq),
    close(S).

edition_stat_file(StatFile) :-
    test_dir(Name),
    test_iteration(N),
    get_results_dir(Name, RDir),
    stat_test_results_dir(RDir, StatF),
    atom_number(A, N),
    atom_concat(A, '.stat', F),
    path_concat(StatF, F, StatFile).

% ----------------------------------------------------------------
:- data test_iteration/1.
inc_test_iteration(N1) :-
    retract_fact(test_iteration(N)),
    N1 is N+1,
    set_fact(test_iteration(N1)).

dump_gat :-
    test_dir(Name),
    test_iteration(N),
    it_dump_gat_file(Name, N, DumpDir),
    ( current_pp_flag(intermod, off) ->
        path_concat(DumpDir, Name, N1),
        atom_concat(N1, '.dump', N0),
        dump(N0, [incremental])
    ;
        dump_registries(DumpDir)
    ).

set_dir_dump_lat :- % CurrMod has the full path
    test_dir(Name),
    test_iteration(N),
    it_dump_gat_file(Name, N, DumpDir),
    set_fact(dump_dir(DumpDir)).

:- export(it_dump_gat_file/3).
:- pred it_dump_gat_file(TestId, N, DumpF)
    #"Dumps the global answer table, i.e., the registry
      information in modular analysis of the state @var{N} of the
      sequence of states of test with id @var{TestId}, in file
      @var{DumpF}.".
it_dump_gat_file(TestId, N, F) :-
    it_dump_gat_file_(TestId, N, F),
    ( file_exists(F) -> true ;
        make_directory(F)
    ).

it_dump_gat_file_(TestId, N, DumpDir) :-
    get_results_dir(TestId, RDir),
    gat_test_results_dir(RDir, T),
    atom_number(A, N),
    atom_concat('inc_reg_', A, F),
    path_concat(T, F, DumpDir). % add extension

dump_registries(DumpDir) :- % This registry should contain the GAT
    findall(X, analyzed_module(X), Modules),
    ( % failure-driven loop
      member(Mod, Modules),
        path_concat(DumpDir, Mod, NewBase),
        write_registry_file(NewBase, Mod, quiet), % locate
        fail
    ; true).

% Modules with registry headers and a global answer table
% (avoids "default" modules that the modular analysis does not
% consider during analysis)
analyzed_module(Mod) :-
    registry_headers(Mod, open_mode(_)),
    ( registry(_SgKey, Mod, _RegData) -> true % (we do not care)
    ; fail
    ).

% This predicate is thought to skip steps of sequences (i.e. so the
% test is not forced to start with 0 clauses)
limit_sequence(OrigSeq, StepStart, NSteps, Seq) :-
    ( StepStart = no ->
        Seq2 = OrigSeq
    ;
        remove_first_elems_list(OrigSeq, StepStart, Seq2)
    ),
    ( NSteps = no ->
        Seq = Seq2
    ;
        get_first_elems_list(Seq2, NSteps, Seq)
    ).

:- export(remove_first_elems_list/3).
:- pred remove_first_elems_list(Ls, N, NLs) : (list(Ls), int(N)) => list(NLs)
    #"Removes the first @var{N} elems of @var{Ls}.".
remove_first_elems_list(Ls, N, NLs) :-
    remove_first_elems_list_(Ls, 0, N, NLs).

remove_first_elems_list_(S, I, I, S) :- !.
remove_first_elems_list_([_|Ls1], I, LsStart, Ls) :-
    I1 is I + 1,
    remove_first_elems_list_(Ls1, I1, LsStart, Ls).

:- export(get_first_elems_list/3).
get_first_elems_list(_, 0, []) :- !.
get_first_elems_list([E|Ls], N, [E|NLs]) :-
    N1 is N - 1,
    get_first_elems_list(Ls, N1, NLs).

decide_make_dir(Dir) :-
    \+ file_exists(Dir), !,
    make_directory(Dir).
decide_make_dir(_).
