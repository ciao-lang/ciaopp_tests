:- module(_,_,[assertions]).

:- use_module(ciaopp(raw_printer)).

:- use_module(ciaopp(ciaopp)).
:- use_module(ciaopp(plai/domains)).
:- use_module(ciaopp(plai/tarjan)).
:- use_module(ciaopp(plai/plai_db)).
:- use_module(ciaopp(plai/incanal/incanal_driver)).
:- use_module(library(aggregates)).
:- use_module(library(terms_vars)).

:- use_module(library(compiler/p_unit/program_keys)).

%:- use_module(ciaopp(plai/incanal/tarjan_inc), [tarjan_data/1]).

:- use_module(engine(io_basic)).

% for now the predicates that would only need to be exported for testing
% purposed are imported in a low level way.

:- import(incanal_driver, [mark_bu_rec_update_complete/5,get_new_answers/3,bottom_up_reanalyze_SCC/4,bu_changed_complete/7]).

% this is a module mainly to test the predicates of the bottom_up
% IMPORTANT: clean properly the incanal db after each test.

%:- test bu_update_complete(X, AbsInt, ChSg, PrimeSg, Sg, NewPrime) + (not_fails, is_det).

% TODO: rename by bu_update_scc_complete (just testing one scc, make a different
% one for the whole program).
:- test bu_update_complete(X, AbsInt, ChSg, ChPrime, NewSg, NewPrime)
   :  ( X = 'bench/mon/bottom_up_tester/bu_test.pl', AbsInt = shfr,
        ChSg = 'bu_test_aux:aux'(A), ChPrime = [([],[A/g])],
        NewSg = 'bu_test_aux:aux'(B) )
   => ( NewPrime = [([],[B/g])] )
   + (not_fails, is_det).

:- test bu_update_complete(X, AbsInt, ChSg, ChPrime, NewSg, NewPrime)
   :  ( X = 'bench/mon/bottom_up_tester/bu_test.pl', AbsInt = shfr,
        ChSg = 'bu_test_aux:aux'(A), ChPrime = [([],[A/g])],
        NewSg = 'bu_test:a'(B) )
   => ( NewPrime = [([],[B/g])] )
   + (not_fails, is_det).

:- test bu_update_complete(X, AbsInt, ChSg, ChPrime, NewSg, NewPrime)
   :  ( X = 'bench/mon/bottom_up_tester/bu_test.pl', AbsInt = shfr,
        ChSg = 'bu_test_aux:aux'(A), ChPrime = [([],[A/g])],
        NewSg = 'bu_test:b'(B) )
   => ( NewPrime = [([],[B/g])] )
   + (not_fails, is_det).

:- test bu_update_complete(X, AbsInt, ChSg, ChPrime, NewSg, NewPrime)
   :  ( X = 'bench/mon/bottom_up_tester/bu_test.pl', AbsInt = shfr,
        ChSg = 'bu_test_aux:aux'(A), ChPrime = [([],[A/g])],
        NewSg = 'bu_test:c'(B) )
   => ( NewPrime = [([],[B/g])] )
   + (not_fails, is_det).

:- test bu_update_complete(X, AbsInt, ChSg, ChPrime, NewSg, NewPrime)
   :  ( X = 'bench/mon/bottom_up_tester/bu_test.pl', AbsInt = shfr,
        ChSg = 'bu_test_aux:aux'(A), ChPrime = [([],[A/g])],
        NewSg = 'bu_test:main2'(B) )
   => ( NewPrime = [([],[B/g])] )
   + (not_fails, is_det).

%% :- test bu_update_complete(X, AbsInt, ChSg, ChPrime, Sg, NewPrime)
%%    : ( X = 'bench/mon/bottom_up_tester/bu_test.pl', AbsInt = shfr,
%%      ChSg = 'bu_test_aux:aux'(A), ChPrime = ([],[A/nf]) )
%%    => ( ChSg = 'bu_test:main2'(B), NewPrime = ([],[B/nf]) )
%%    + (not_fails, is_det).

%% :- test bu_update_complete(X, AbsInt, ChSg, ChPrime, Sg, NewPrime)
%%    : ( X = 'bench/mon/bottom_up_tester/bu_test.pl', AbsInt = shfr,
%%      ChSg = 'bu_test_aux:aux'(A), ChPrime = ([],[A/f]) )
%%    => ( ChSg = 'bu_test:main2'(B), NewPrime = ([],[B/f]) )
%%    + (not_fails, is_det).

% This test loads a module and arbitrarily changes a complete, NewPrime contains
% the Prime of a predicate `Sg` with the topmost abstraction.
bu_update_complete(File, AbsInt, ChSg, ChPrime, NewSg, NewPrime) :-
    set_pp_flag(fixpoint, dd),
    set_pp_flag(trace_fixp, trace),
    clean_analysis_info,
    init_empty_inc_analysis,
    module(File),
    analyze(AbsInt),
    show_analysis,
    set_pp_flag(del_strategy, bottom_up_cls),
    predkey_from_sg(ChSg,ChSgKey),
    varset(ChSg, VarsCh),
    unknown_entry(AbsInt,ChSg,VarsCh,ChProj),
    complete(ChSgKey,AbsInt,Sg1,Proj1,_Prime1,Id1,_), % backtracking here
    identical_proj(AbsInt, ChSg, ChProj, Sg1, Proj1), !,
    % mark that the complete changed
    mark_bu_rec_update_complete(Id1, ChSgKey, AbsInt, ChSg, ChPrime),
    % redo analysis (lowlevel)
    recursive_classes(SCCs),
    findall(comp(SgKey,Sg,Proj,Prime,Id,Fs),
            bu_changed_complete(SgKey,Sg,AbsInt,Proj,Prime,Id,Fs),
            Completes),
    get_new_answers(Completes, AbsInt, NewAnswers),
    set_pp_flag(trace_fixp, trace),
    bottom_up_reanalyze_SCC(Completes, NewAnswers,AbsInt, SCCs),
    display(ended_bu), nl,
    % get result
    predkey_from_sg(NewSg,NewSgKey),
    varset(NewSg, NewVars),
    unknown_entry(AbsInt,NewSg,NewVars,NewProj),
    complete(NewSgKey,AbsInt,Sg2,Proj2,Prime2,_Id2,Fs), % backtracking here
    identical_proj(AbsInt, NewSg, NewProj, Sg2, Proj2), !,
    NewPrime = Prime2.