:- use_module(library(bundle/bundle_paths),[bundle_path/3]).
:- use_module(library(pathnames), [path_concat/3]).

:- doc(section, "Benchmarks").
% test_dir(TopLevel, BenchId, DirName, EditionSimulationType).
test_dir(TopLevel, BenchId, Dir, manual) :-
    basic_test(BenchId,TopLevel), !,
    basic_incanal_dir(BenchId, Dir).
% bundles (they need to be deactivated in your local copy)
% This is not autmatically sync, it could be done by reading at the Manifest
test_dir(BundleEntry, Bundle, Dir, bundle) :-
    bundle_test(Bundle, BundleEntry), !,
    bndls_dir(BundlesDir),
    path_concat(BundlesDir,Bundle,Dir).
    % assertions

:- doc(section, "Tests cases").
% types
test_dir(T, X, Dir, manual) :-
    test_case(X,T),
    basic_incanal_dir(X,Dir).
test_dir('trust_success.pl',X,Dir,states) :-
    inc_trust_succ_test(X), !,
    incanal_trusts_dir(X,Dir).
test_dir(trust_calls,X, Dir,states) :-
    inc_trust_call_test(X), !,
    incanal_trusts_dir(X,Dir).
test_dir(trust,X, Dir,git) :-
    basic_inc_trust_test(X), !,
    incanal_trusts_dir(X,Dir).

:- pred basic_test(TestId,TestTopLevel) => atm * atm.

basic_test(qsort, test).
basic_test(mmutr, m1).
basic_test(hanoi, hanoi).
basic_test(ann, ann).
basic_test(aiakl, aiakl).
basic_test(bid, zbid).
basic_test(boyer, z_boyer).
basic_test(simple, simple).
basic_test(manag_proj_simple, managing_project).
basic_test(check_links, main_check_links).
basic_test(peephole, peephole).
basic_test(prolog_read, z_prolog_read).
basic_test(witt, witt).
basic_test(progeom, progeom).
basic_test(rdtok, rdtok).
basic_test(warplan, warplan).

% types
test_case(min_types, main).
% mutually recursive
test_case(mmutr, m1).
% renaming
test_case(renaming_test,ren).
% monolithic
test_case(hanoi_mon, hanoi).
% bu delete clauses
test_case(renaming_test, ren).
test_case(parents1, parents_test).
test_case(parents2, parents_test).

:- pred bundle_test(BundleId,BundleTopLevel) => atm * atm.

% bundle entry may be overwritten by the configuration file
bundle_test(gendot,'lib/gendot/gendot.pl').
bundle_test(leancop,'src/leancop_main').
bundle_test(ciao_gui,'ciao_gui/cmds/ciao_gui.pl').
bundle_test(ciaofmt,'cmds/ciaofmt').
bundle_test(bibutils,'cmds/pl2pubs.pl').
bundle_test(cleandirs,'cmds/cleandirs').
bundle_test(synch_actions,'cmds/synch_actions').
bundle_test(tptp2X,'src/tptp2X.main.pl'). % no entries, add exports?
bundle_test(provrml,'lib/provrml/provrml.pl').
bundle_test(mycin,'lib/mycin/mycin_support.pl').
bundle_test(xml_extra,'tests/xdr_handle_test/xdr_handle_test.pl'). % more tests available
bundle_test(lpmake,'cmds/lpmake.pl').
bundle_test(lpdoc,'cmds/lpdoccl.pl').
bundle_test(davinci,'lib/davinci/davinci.pl').

basic_incanal_dir(Id,Dir) :-
    path_concat('tests/incanal/bench',Id,D1),
    bundle_path(ciaopp_tests, D1, Dir).

mon_incanal_dir(Id,Dir) :-
    path_concat('tests/incanal/bench/mon',Id,D1),
    bundle_path(ciaopp_tests, D1, Dir).

basic_inc_trust_test(trust_calls).
basic_inc_trust_test(trust_calls_complex).

inc_trust_call_test('trust_C1_+').
inc_trust_call_test('trust_C2_+').
inc_trust_call_test('trust_C3_+').
inc_trust_call_test('trust_C1_-').
inc_trust_call_test('trust_C2_-').
inc_trust_call_test('trust_C3_-').
inc_trust_call_test('trust_C1_-bot').
inc_trust_call_test('trust_C1_+bot').
inc_trust_call_test('trust_C_exported').
inc_trust_call_test('trust_C_internal').

inc_trust_succ_test('trust_S1').
inc_trust_succ_test('trust_S2').
inc_trust_succ_test('trust_S3').
inc_trust_succ_test('trust_S4').

%incanal_trusts_dir(Id,Dir) :- !, fail.
incanal_trusts_dir(Id,Dir) :-
    path_concat('tests/incanal/bench',Id,D1),
    bundle_path(ciaopp_tests, D1, Dir).
