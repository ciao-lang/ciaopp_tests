:- use_module(library(bundle/bundle_paths),[bundle_path/3]).
:- use_module(library(pathnames), [path_concat/3]).

% test_dir(TopLevel, BenchId, DirName).
test_dir(test, qsort, Dir, manual) :-
        basic_incanal_dir(qsort,Dir).
test_dir(m1, mmutr, Dir, manual) :-
        basic_incanal_dir(mmutr,Dir).
test_dir(hanoi, hanoi, Dir, manual) :-
        basic_incanal_dir(hanoi,Dir).
test_dir(ann, ann, Dir, manual) :-
        basic_incanal_dir(ann,Dir).
test_dir(aiakl, aiakl, Dir, manual) :-
        basic_incanal_dir(aiakl,Dir).
test_dir(zbid, bid, Dir, manual) :-
        basic_incanal_dir(bid,Dir).
test_dir(z_boyer, boyer, Dir, manual) :-
        basic_incanal_dir(boyer,Dir).
test_dir(simple, simple, Dir, manual) :-
        basic_incanal_dir(simple,Dir).
% test_dir(mchat, chat80, Dir, manual) :-
%       basic_incanal_dir(chat80,Dir).
test_dir(peephole, peephole, Dir, manual) :-
        basic_incanal_dir(peephole,Dir).
test_dir(z_prolog_read, prolog_read, Dir, manual) :-
        basic_incanal_dir(prolog_read,Dir).
test_dir(witt, witt, Dir, manual) :-
        basic_incanal_dir(witt,Dir).
test_dir(progeom, progeom, Dir, manual) :-
        basic_incanal_dir(progeom,Dir).
test_dir(rdtok, rdtok, Dir, manual) :-
        basic_incanal_dir(rdtok,Dir).
test_dir(warplan, warplan, Dir, manual) :-
	basic_incanal_dir(warplan,Dir).
test_dir(main_cleandirs, cleandirs, Dir, manual) :-
        basic_incanal_dir(cleandirs,Dir).
test_dir(main_leancop, leancop, Dir, manual) :-
        basic_incanal_dir(leancop,Dir).
test_dir(bu_doctree,bibutils, Dir, manual) :-
        basic_incanal_dir(bibutils,Dir).
% for incverif paper
% TODO: hardwired path!
test_dir('cmds/lpdoccl.pl',lpdoc_backends,'~/clip/Papers/incverif/incanal_assrts_paper/src/lpdoc_backends',states).
% test_dir('cmds/lpdoccl.pl',lpdoc_asr_inc,'~/clip/Papers/incverif/incanal_assrts_paper/src/lpdoc_asr_inc',states).

% assertions
test_dir('trust_success.pl',X,Dir,states) :-
        inc_trust_succ_test(X), !,
        incanal_trusts_dir(X,Dir).
test_dir('trust_calls.pl',X, Dir,states) :-
        inc_trust_call_test(X), !,
        incanal_trusts_dir(X,Dir).
test_dir(trust,X, Dir,git) :-
        basic_inc_trust_test(X), !,
        incanal_trusts_dir(X,Dir).

basic_incanal_dir(Id,Dir) :-
        path_concat('tests/incanal/bench',Id,D1),
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

inc_trust_succ_test('trust_S1').
inc_trust_succ_test('trust_S2').
inc_trust_succ_test('trust_S3').
inc_trust_succ_test('trust_S4').

%incanal_trusts_dir(Id,Dir) :- !, fail.
incanal_trusts_dir(Id,Dir) :-
        path_concat('tests/incanal/bench',Id,D1),
        bundle_path(ciaopp_tests, D1, Dir).
