:- module(_, [test/2], [assertions, isomodes, nativeprops, dynamic, fsyntax]).

:- doc(title, "Incremental analysis tester").

:- doc(module, "This module contains the functionality to perform test of analysis.

This is a low-level interface, for analyzing consistently with ciaopp
flags, use the incanal_intermod_bench_driver module.").

:- use_module(library(sort), [sort/2]).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(pathnames)).
:- use_module(library(fastrw), [fast_write/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3,reverse_bundle_path/3]).
:- use_module(library(arithpreds), [ceiling/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(random), [srandom/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(engine(runtime_control), [garbage_collect/0]).
:- use_module(library(streams)).
:- use_module(library(format), [format/2]).
:- use_module(library(messages), [error_message/2,show_message/3]).
:- use_module(library(source_tree), [current_file_find/3, copy_file_or_dir/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).
:- use_module(engine(internals), [ciao_root/1]).
:- use_module(library(operators)).
:- use_module(library(hiordlib), [maplist/3]).

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(ciaopp(analyze_driver), [analyze/2]).
:- use_module(ciaopp(frontend_driver), [module/1]).
:- use_module(ciaopp(plai/intermod), [intermod_analyze/3]).
:- use_module(ciaopp(p_unit/p_dump), [dump_dir/1, dump/2]).
:- use_module(ciaopp(plai/intermod_punit), [write_registry_file/3]).
:- use_module(ciaopp(plai/intermod_db), [registry/3, registry_header/2]).
:- use_module(ciaopp(raw_printer)).

:- use_module(ciaopp(analysis_stats), [clean_stat_steps/0, dump_steps/1]).
:- use_module(ciaopp_tests(incanal/naive_reader)).
:- use_module(ciaopp_tests(incanal/edition_sequence_generator)).
:- use_module(ciaopp_tests(incanal/module_writer)).
:- use_module(ciaopp_tests(incanal/config_db)).
:- use_module(ciaopp_tests(incanal/git_wrapper)).

:- export(all_tests_results_dir/1).
all_tests_results_dir(Dir) :-
    bundle_path(ciaopp_tests, 'tests/incanal/test_results', Dir),
    decide_make_dir(Dir).

code_test_all_config_dir(Dir, Id, CDir) :-
    ( get_test_config('--edit_type', add) ->
        ET = add, DE = []
    ;
        current_pp_flag(del_strategy, Del),
        ET = del, DE = ['-', Del]
    ),
    ( current_pp_flag(incremental, on) -> Inc = inc ; Inc = noninc),
    atom_number(NEAtm, ~get_test_config('--n_edits')),
    ( get_test_config('--user_tag', UId0) -> UIdL = ['-', UId0] ; UIdL = []),
    ( get_test_config('--rand', Seed) ->
        atom_number(SeedAtm, Seed),
        Rand = [rand, '-', SeedAtm|X]
    ;   Rand = [not_rand|X]
    ),
    current_pp_flag(fixpoint, FP),
    ( current_pp_flag(module_loading, all) -> Mod = mon ; Mod = mod ),
    X = ['-',NEAtm,'-',~get_test_config('--domain'),'-',FP|UIdL],
    D1 = ~path_concat(Dir, ~atom_concat([Id,'-',ET,'-'|Rand])),
    decide_make_dir(D1),   % bundle test dir
    CDir = ~path_concat(D1, ~atom_concat([Mod,'-',Inc|DE])),
    decide_make_dir(CDir). % config dir

:- pred get_results_dir(BenchId, TDir) #"@var{TDir} is the location of results
   in this bundle. A directory is created.".
get_results_dir(BenchId, TDir) :-
    get_test_config('--config-exp',CFile), !,
    path_dirname(CFile, Base),
    get_file_config(experiment_dir(RelDir)),
    code_test_all_config_dir(~path_concat_list([Base,RelDir]), BenchId, TDir).
get_results_dir(BenchId, TDir) :-
    code_test_all_config_dir(~all_tests_results_dir, BenchId, TDir).

:- export(gat_test_results_dir/2).
gat_test_results_dir(RDir, GTDir) :-
    path_concat(RDir, 'detailed_step_results', GTDir).

stat_test_results_dir(RDir, STDir) :-
    path_concat(RDir, 'stats', STDir).

code_db_from_dir(BaseDir) :-
    clean_reader,
    test_dir(BenchId,_),
    set_bundle_ops,
    ( % failure-driven loop
      test_file(BaseDir,BenchId,ModPath),
        path_splitext(~path_basename(ModPath),ModName,'.pl'),
        \+ naive_loaded(ModName,_),
        show_message(simple, 'Reading ~w~n',[ModPath]),
        read_module(ModName, ModPath),
        fail
    ; true ).

:- pred test_file(+,+,-). % enumerates the files to be tested -- included files not working
% TODO: get also included files (although not present in the current benchmarks)
test_file(BaseDir,BenchId,File) :-
    file_config(edit_dir(D)), % backtracking here
    path_concat(~path_concat(BaseDir,BenchId),D,Dir),
    current_file_find([proj(compilable), srctype(module)], Dir, File).
test_file(BaseDir,_,File) :-
    \+ file_config(edit_dir(_)),
    current_file_find([proj(compilable), srctype(module)], BaseDir, File).

set_bundle_ops :-
    file_config(bundle_op(A,B,C)),
    op(A,B,C),
    fail.
set_bundle_ops.

:- pred test(BenchId, Opts) + (not_fails, is_det)
   #"Runs a test of analysis. The configuration has is expressed as follows:

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
   @end{itemize}  ".
test(BenchId, Opts) :-
    set_configs(Opts),
    bench_config(BenchId,SrcDir,TopLevel,DirType),
    set_fact(test_it(0)),
    set_fact(test_toplevel(TopLevel)),
    get_analysis_dir(BenchId,AnaDir), % directory where analysis is performed
    get_results_dir(BenchId,ResDir),  % directory where results are stored
    init_results_directories(ResDir), % move before generating sim seq
    set_fact(test_dir(BenchId,SrcDir)),
    generate_simulation_sequence(DirType,SrcDir,Seq),
    write_sequence_file(ResDir,Seq),
    ( get_test_config('--debug-steps', _) ->
        write_dir_state_sequence(Seq, DirType,ResDir)
    ;   true
    ),
    clean_ana_dir(AnaDir),
    ( (test_dir(TopLevel,BenchId,_,bundle) ; get_file_config(bundle_location(_))) ->
        activate_bundle(BenchId,AnaDir,SrcDir)
    ; true ),
    perform_seq_analysis(Seq,DirType,~path_concat(AnaDir,TopLevel),AnaDir).

set_configs([]).
set_configs([C|Cs]) :-
    retract_fact(test_config(C, _)), !,
    Cs = [Value|Cs2],
    assertz_fact(test_config(C, Value)),
    set_configs(Cs2).
set_configs([C|Cs]) :-
    error_message("Option not available: ~w", [C]),
    set_configs(Cs).

bench_config(BenchId,SrcDir,TopLevel,manual) :-
    get_file_config(bundle_location(RelSrcDir)), !, % configuration using file
    bndls_dir(BundlesDir),
    path_concat(BundlesDir,RelSrcDir,SrcDir),
    get_file_config(entry_module(RTL)),
    path_concat(BenchId,RTL,TopLevel).% assuming only one for now
bench_config(BenchId,SrcDir,TopLevel,DirType) :-
    test_dir(TopLevel,BenchId,SrcDir,DirType).

get_analysis_dir(BenchId,AnaDir) :- % TODO: hardwired -> remove now?
    atom_concat('lpdoc',_,BenchId), !,
    bundle_path(lpdoc_asr_inc, '.', AnaDir).
get_analysis_dir(BenchId,AnaDir) :-
    get_results_dir(BenchId,AnaDir).

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

% -----------------------------------------------------------------
:- doc(section, "Prepare test edition sequence").
% -----------------------------------------------------------------

:- pred generate_simulation_sequence/3 + (not_fails, is_det).
generate_simulation_sequence(manual, SrcDir, Seq) :-
    code_db_from_dir(SrcDir),
    get_code_summary(Sum0, NCls),
    prioritize_modules(Sum0,Sum),
    get_test_config('--n_edits', NEdits),
    generate_sim_seq_(Sum, NCls, ~ceiling(NCls/NEdits), NEdits, Seq, EditType),
    fill_seq(Seq, ~complementary_edit_type_(EditType)).
generate_simulation_sequence(git, SrcDir, Seq) :-
    load_git_repo(SrcDir,incanal_git),
    % currenlty for git repos, only the order of commit is available
    findall(Commit, git_log(incanal_git,_,Commit), Seq),
    ( reverse_bundle_path(SrcDir,_,_) ->
        clean_ana_dir(SrcDir)
    ; true
    ).
generate_simulation_sequence(states,SrcDir,Seq) :-
    sort(~directory_files(SrcDir),Seq1),
    findall(X, filter_directories(Seq1,SrcDir,X), Seq).

filter_directories(Seq, SrcDir, X) :-
    member(D,Seq),
    ( atom_concat('state_', _, D) -> true ; fail ),
    path_concat(SrcDir,D,X).

% edit not_rand mod_by_mod
generate_sim_seq_(Sum, NCls, Steps, NEdits, Seq, 1) :-
    get_seq_limits(NCls,PreSkip0,MaxS),
    get_test_config('--edit_type', EditT),
    ( EditT = del -> PreSkip = ~(NCls-PreSkip0-MaxS) ; PreSkip = PreSkip0 ),
    Cfg = cfg(NCls, Steps, NEdits,1,PreSkip,MaxS),
    ( get_test_config('--rand', Seed) ->
        srandom(Seed),
        gen_edit_sequence(Sum, Cfg, gen_random_sorted_sequence, Seq)
    ;
        ( get_edit_mode(predicate) ->
            gen_edit_sequence(Sum, Cfg, gen_pred_num, Seq1)
        ;
            gen_edit_sequence(Sum, Cfg, gen_num_sequence, Seq1)
        )
    ),
    ( EditT = del -> reverse(Seq1, Seq) ; Seq = Seq1).

edit_number(add,1).
edit_number(del,0).

get_seq_limits(TotalSteps, Start, Steps) :-
    get_test_config('--seq_sz', abs), !,
    ( get_test_config('--start', Start) -> true ; Start = 0 ),
    ( get_test_config('--steps', Steps) -> true ; Steps is TotalSteps - Start).
get_seq_limits(TotalSteps, Start, Steps) :-
    ( get_test_config('--start', StartRatio) ->
        Start = ~ceiling((StartRatio/100) * TotalSteps)
    ; Start = no ),
    ( get_test_config('--steps', StepsRatio) ->
        Steps = ~ceiling((StepsRatio/100) * TotalSteps)
    ; Steps = no
    ).

prioritize_modules(Sum,Sum) :-
    ( \+ get_file_config(priority_modules(_)) ;
        get_file_config(priority_modules([])) ), !.
prioritize_modules(Sum0,Sum) :-
    get_file_config(priority_modules(Prior)),
    maplist(tr_sum,Prior,PriorSum), % create list with the shape of the summary
    % remove priority modules and sort the rest
    append(PriorSum,~sort(~difference(Sum0,PriorSum)), Sum).

tr_sum(X,X-_).

complementary_edit_type_(1,0).
complementary_edit_type_(0,1).

fill_seq([], _).
fill_seq([St|Seq], EditType) :-
    fill_dir(St, EditType),
    fill_seq(Seq, EditType).

fill_dir([], _).
fill_dir([p(_, ClList)|IniState], EditType) :-
    ( get_edit_mode(predicate) ->
        fill_pred(ClList, EditType)
    ;
        fill_list(ClList,EditType)
    ),
    fill_dir(IniState, EditType).

:- export(fill_pred/2).
fill_pred([], _).
fill_pred([_P/_A-LCls|Xs], EditType) :- !,
    fill_list(LCls,EditType),
    fill_pred(Xs, EditType).

fill_list([], _).
fill_list([X|Xs], EditType) :-
    X = EditType, !,
    fill_list(Xs, EditType).
fill_list([_|Xs], EditType) :-
    fill_list(Xs, EditType).

% -----------------------------------------------------------------
:- doc(section, "Perform analysis").
% -----------------------------------------------------------------

:- pred perform_seq_analysis/4 + (not_fails, is_det).
perform_seq_analysis([], _, _, _).
perform_seq_analysis([St|Seq], DirType, TopLevel, Dir) :-
    perform_seq_analysis1(St, DirType, TopLevel, Dir), !,
    % TODO: This cut removes choicepoints (the choicepoints were making
    % retracting facts very slow, because it had to check them)
    perform_seq_analysis(Seq, DirType, TopLevel, Dir).

:- pred perform_seq_analysis1/4 + (not_fails, is_det).
perform_seq_analysis1(St, DirType, TopLevel, Dir) :-
    write_dir_state(St, DirType, Dir,git_checkout_copy),
    get_test_config('--domain', AbsInt),
    clean_stat_steps,
    format('~n---------- Edition iteration ~d ----------~n~n', [~inc_test_it]),
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
    dump_steps(~edition_stat_file),
    dump_gat.

write_sequence_file(DstDir, Seq) :-
    open(~path_concat(DstDir, 'seq.bin'), write, S),
    fast_write(S, Seq),
    close(S).

edition_stat_file(StatFile) :-
    test_dir(Name,_),
    stat_test_results_dir(~get_results_dir(Name), StatF),
    atom_number(A, ~test_it),
    path_concat(StatF, ~atom_concat(A, '.stat'), StatFile).

init_results_directories(ResDir) :-
    gat_test_results_dir(ResDir, GDir),
    catch(make_directory(GDir), _, dir_exists_handler(GDir)),
    stat_test_results_dir(ResDir, SDir),
    catch(make_directory(SDir), _, dir_exists_handler(SDir)).

dir_exists_handler(Dir) :-
    error_message("Dir exists, remove it to redo the test: \n~w",[~path_dirname(Dir)]),
    abort.

% ----------------------------------------------------------------
:- data test_it/1.
inc_test_it(N) :-
    retract_fact(test_it(N)),
    set_fact(test_it(~(N+1))).

dump_gat :-
    test_dir(Name,_),
    it_dump_gat_file(Name, ~test_it, DumpDir),
    ( current_pp_flag(intermod, off) ->
        dump(~atom_concat(~path_concat(DumpDir, Name), '.dump'), [incremental])
    ;
        dump_registries(DumpDir)
    ).

set_dir_dump_lat :- % CurrMod has the full path
    test_dir(Name,_),
    set_fact(dump_dir(~it_dump_gat_file(Name, ~test_it))).

:- export(it_dump_gat_file/3).
:- pred it_dump_gat_file(BenchId, N, DumpF)
   #"Dumps the global answer table, i.e., the registry
     information in modular analysis of the state @var{N} of the
     sequence of states of test with id @var{BenchId}, in file
     @var{DumpF}.".
it_dump_gat_file(BenchId, N, DumpDir) :-
    atom_number(A, N),
    path_concat(~gat_test_results_dir(~get_results_dir(BenchId)),
                ~atom_concat('inc_reg_', A),
                DumpDir),
    decide_make_dir(DumpDir).

dump_registries(DumpDir) :- % This registry should contain the GAT
    ( % failure-driven loop
      analyzed_module(Mod),
        write_registry_file(~path_concat(DumpDir, Mod), Mod, quiet),
        fail
    ; true).

% Modules with registry headers and a global answer table
% (avoids "default" modules that the modular analysis does not
% consider during analysis)
analyzed_module(Mod) :-
    registry_header(Mod, open_mode(_)),
    ( registry(_SgKey, Mod, _RegData) -> true % (we do not care)
    ; fail
    ).

decide_make_dir(Dir) :-
    \+ file_exists(Dir), !,
    make_directory(Dir).
decide_make_dir(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, "Bundle").

:- use_module(ciaobld(bundle_scan), [scan_bundles_at_path/1]).
:- use_module(ciaopp_tests(incanal/dynamic_workspace)).

activate_bundle(BenchId,AnaDir,SrcDir) :-
    path_concat(SrcDir,BenchId,BndlDir),
    path_concat(AnaDir,BenchId,BndlAnaDir),
    %% copy manifest file or directory
    copy_file_or_dir(~path_concat(BndlDir,'Manifest'),BndlAnaDir),
    %% copy the packages
    ( % failure-driven loop
        ( current_file_find([srctype(package)], BndlDir, File) ;
            current_file_find([srctype(include)], BndlDir, File) ),
        path_relocate(BndlDir,BndlAnaDir,File,AnaFile),
        process_call(path(mkdir), ['-p', ~path_dirname(AnaFile)], []),
        copy_file_or_dir(File,AnaFile),
        fail
    ; true),
    show_message(simple, 'Activating bundle ~w at~n~t ~w~n',[BenchId,AnaDir]),
    activate_workspace(AnaDir),
    reload_ciaopath,
    scan_bundles_at_path(AnaDir).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% :- op(970, fx, (nochpt)).
%% :- use_module(engine(hiord_rt), [call/1]).
%% % TODO: move??
%% :- meta_predicate nochpt(goal).
%% :- export(nochpt/1).
%% nochpt(G) :-
%%     '$metachoice'(Chpt0),
%%     call(G),
%%     '$metachoice'(Chpt),
%%     ( Chpt == Chpt0 -> true
%%     ; error_message("WARNING: expected NOCHPT!! ~q", [G])
%%     ).