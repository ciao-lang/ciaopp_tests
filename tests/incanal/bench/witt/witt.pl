:- module(witt, [witt/1], [assertions]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% ml.pl -- main file fot the WITT clustering system. To start, simply
 %% load this file and call witt/0. It will take some seconds to complete.
 %% Universe to be clustered in examples.pl.
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro Li~nares
 %% Created On      : At some point in the year 92/93
 %% Last Modified By: MCL
 %% Last Modified On: Wed Feb 26 18:32:32 2003
 %% Update Count    : 89
%% Status          : Correct and working

% modularized by IG

:- use_module(library(write), [write/1]).
:- use_module(engine(io_basic)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [length/2]).

:- use_module(universe, [attribute/2, example/2]).
:- use_module(operations, [dij/2, wc/2, distance/3, cc/3, combine/3]).
:- use_module(my_library, [my_own_ordunion/3, list_to_my_own_ordset/2,
    my_own_select_/3, my_own_memberchk/2, my_own_append/3,
    non_my_own_member/2, my_own_member/2]).

% Clustering parameters
factor(1.2).

threshold2(0.05).
threshold3(1.0).

%:- entry witt/1 : var.

witt(X) :-
    witt_nw(X).

witt_nw(Taxo):-
    Taxo = [Initial, First_Partition|Classes],
    universe(World),
    my_own_select_names(World, Initial),
    precluster(World, First_step),
    my_own_select_names(First_step, First_Partition),
    split(First_step, Instances, Categories),
    refinement(Instances, Categories, Classes).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% The actual algorithm follows. I'm not going to explain it
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

precluster(World, NewWorld):-
    smallest_dist(World, O1, O2, D),
    factor(F),
    T1 is F*D,
    compare(C, D, T1),
    precluster(C, World, O1, O2, T1, NewWorld).

precluster(>, W, _, _, _, W).
precluster(=, W, _, _, _, W).
precluster(<, W, O1, O2, T1, Nw):-
    my_own_select_(O1, W, W1),
    my_own_select_(O2, W1, W2),
    precluster(W2, O1, O2, T1, Nw).
precluster([], O1, O2, _, [O1, O2]).
precluster([W|Ws], O1, O2, T1, Nw):-
    combine(O1, O2, Category),
    UpW = [Category, W|Ws],
    smallest_dist(UpW, Ob1, Ob2, D),
    compare(C, D, T1),
    precluster(C, UpW, Ob1, Ob2, T1, Nw).

smallest_dist(World, O1, O2, Best):-
    World = [Ob1, Ob2|_],
    distance(Ob1, Ob2, D),
    smallest_dist(World, Ob1, Ob2, D, O1, O2, Best).
smallest_dist([], O1, O2, D, O1, O2, D).
smallest_dist([W|Ws], Ob1, Ob2, D, O1, O2, Best):-
    smallest_dist(Ws, W, Ob1, Ob2, D, Oi1, Oi2, Di),
    smallest_dist(Ws, Oi1, Oi2, Di, O1, O2, Best).
smallest_dist([], _, O1, O2, Best, O1, O2, Best).
smallest_dist([Ob|Obs], Object, Ob1, Ob2, D, O1, O2, Best):-
    distance(Ob, Object, Di),
    compare(C, D, Di),
    my_own_select_tri(C,  Ob, Object, Di, Ob1, Ob2, D,NOb1, NOb2, Nd),
    smallest_dist(Obs, Object, NOb1, NOb2, Nd, O1, O2, Best).

% Not used
write_taxo([], _).
write_taxo([T], N):- !,
    write(level(N)), nl,
    write_clas(T).
write_taxo([T, T1|Ts], N):-
    write(level(N)), nl,
    write_clas(T),
    N1 is N + 1,
    write_taxo([T1|Ts], N1).
write_clas([T]):- write(T), nl, nl.
write_clas([T, T1|Ts]):-
    write(T),
    write(', '),
    write_clas([T1|Ts]).

    
my_own_select_names(O, N):-
    my_own_select_names(O, [], N).
my_own_select_names(ml(N, _, _, _), I, [N|I]).
my_own_select_names(pair(O1, O2), I, N):-
    my_own_select_names(O1, I, M),
    my_own_select_names(O2, M, N).
my_own_select_names([], N, N).
my_own_select_names([ml(N, _, _, _)|Os], A, Ns):-
    my_own_select_names(Os, [N|A], Ns).

extract_instances(Ins, Addable, True):-
    my_own_select_names(Addable, AdNames),
    plain(AdNames, [], Plain),
    extract_instances_1(Ins, Plain, True).
extract_instances_1([], _, []):- !.
extract_instances_1([I|Ins], NCats, TIns):-
    I = ml([N], _, _, _),
    my_own_member(N, NCats),!,
    extract_instances_1(Ins, NCats, TIns).
extract_instances_1([I|Ins], NCats, [I|TIns]):-
    I = ml([N], _, _, _),
    non_my_own_member(N, NCats),!,
    extract_instances_1(Ins, NCats, TIns).

plain([], P, P):- !.
plain([A|As], I, O):- !,
    plain(A, I, M),
    plain(As, M, O).
plain(A, I, [A|I]):-
    atomic(A), !.

split([], [], []).
split([X|Xs], [X|Ys], Zs):-
    instance(X),
    split(Xs, Ys, Zs).
split([X|Xs], Ys, [X|Zs]):-
    category(X),
    split(Xs, Ys, Zs).

instance(ml([_], _, _, _)).
category(ml([_,_|_], _, _, _)).


find_addable(NewCats, Cats, T3, Addable):-
    find_addable(NewCats, Cats, T3, [], Addable).

find_addable([], _, _, A, A).
find_addable([Nc|Ncs], Cats, T3, InA, OutA):-
    is_addable(Cats, Nc, T3, Verdict),
    verdict(Verdict, Nc, InA, MidA),
    find_addable(Ncs, Cats, T3, MidA, OutA).

verdict(yes, Nc, InA, [Nc|InA]).
verdict(no, _, InA, InA).

is_addable([], _, _, yes).
is_addable([C1|Cs], Nc, T3, V):-
    combine(C1, Nc, C2),
    wc(C2, W),
    compare(C, W, T3),
    is_addable(C, Cs, Nc, T3, V).
is_addable(<, Cats, Cat, T3, V):-
    is_addable(Cats, Cat, T3, V).
is_addable(>, _, _, _, no).
is_addable(=, _, _, _, no).

best_cohesion(Cat1, Cat2, Cats, C1, C2, Score):-
    combine(Cat1, Cat2, Cat3),
    wc(Cat3, W3),
    best_cohesion([Cat2|Cats], Cat1, Cat1, Cat2, W3, C1, C2, Score).
best_cohesion([], _, C1, C2, S, C1, C2, S).
best_cohesion([Cat|Cats], C, PCat1, PCat2, PSco, BCat1, BCat2, BSco):-
    combine(Cat, C, NCat),
    wc(NCat, NSco),
    compare(Comp, NSco, PSco),
    my_own_select_tri(Comp, Cat, C, NSco, PCat1, PCat2, PSco, MCat1, MCat2, MSco),
    best_cohesion(Cats, C, MCat1, MCat2, MSco, QCat1, QCat2, QSco),
    best_cohesion(Cats, Cat, QCat1, QCat2, QSco, BCat1, BCat2, BSco).

my_own_select_tri(<, _, _, _, A, B, C, A, B, C).
my_own_select_tri(=, _, _, _, A, B, C, A, B, C).
my_own_select_tri(>, A, B, C, _, _, _, A, B, C).

universe(U):-
    findall(A, attribute(A, _), Ats),
    length(Ats, N),
    Z is N *(N - 1) / 2,
    findall(Instance, instance(Instance, Ats, Z), U).

instance(ml([Name], OrdCars, Z, Mats), Ats, Z):-
    example(Name, Cars),
    intra_order(Cars, IntraCars),
    list_to_my_own_ordset(IntraCars, OrdCars),
    findall(mat(D, Ai, Aj, M), cont_table(D, Ai, Aj, M, Ats, Cars), Mats).

intra_order([], []).
intra_order([A-Vs|Cs], [A-Ovs|Os]):-
    list_to_my_own_ordset(Vs, Ovs),
    intra_order(Cs, Os).

cont_table(D, Ai, Aj, M, Ats, Cars):-
    my_own_append(_, [Ai|Ats1], Ats), attribute(Ai, Vali),
    my_own_member(Aj, Ats1), attribute(Aj, Valj),
    my_own_member(Ai-Vi, Cars),
    my_own_member(Aj-Vj, Cars),
    fill_matrix(Vali, Valj, Vi, Vj, M),
    dij(M, D).

fill_matrix([], _, _, _, []).
fill_matrix([Vali|Valis], Valjs, Presi, Presj, [V|Vs]):-
    fill_vector(Valjs, Vali, Presi, Presj, V),
    fill_matrix(Valis, Valjs, Presi, Presj, Vs).

fill_vector([], _, _, _, []).
fill_vector( [Valj|Valjs], Vali, Presi, Presj, [E|Es]):-
    double_my_own_member_check(Vali, Valj, Presi, Presj, E),
    fill_vector(Valjs, Vali, Presi, Presj, Es).

double_my_own_member_check(E1, E2, L1, L2, 1):-
    my_own_memberchk(E1, L1),
    my_own_memberchk(E2, L2), !.
double_my_own_member_check(_, _, _, _, 0).


refinement(Ins, Cats, Classes):-
    threshold2(T2),
    threshold3(T3),
    refinement(Ins, Cats, T2, T3, Classes).
refinement([], Cats, T2, T3, Classes):-
    compute_within(Cats, [], T2, T3, Classes).
refinement([I|Is], Cats, T2, T3, Classes):-
    classify_pairs([I|Is], Cats, T2, T3, Classes).

classify_pairs(Ins, Cats, T2, T3, Classes):-
    get_best_pair(Ins, Cats, In, Cat, Score),
    compare(C, Score, T2),
    check_add(C, Ins, Cats, In, Cat, T2, T3, Classes).

check_add(>, Ins, Cats, I, Cat, T2, T3, [Partition|Classes]):-
    NewWorld = [NewCat|NewCats],
    combine(I, Cat, NewCat),
    my_own_select_(I, Ins, NewIns),
    my_own_select_(Cat, Cats, NewCats),
    my_own_select_names(pair(NewIns, NewWorld), Partition),
    refinement(NewIns, NewWorld, T2, T3, Classes).
check_add(=, Ins, Cats, _, _, T2, T3, Classes):-
    new_categories(Ins, Cats, T2, T3, Classes).
check_add(<, Ins, Cats, _, _, T2, T3, Classes):-
    new_categories(Ins, Cats, T2, T3, Classes).


new_categories([], Cats, T2, T3, Classes):-            %% No examples
    compute_within(Cats, [], T2, T3, Classes).
new_categories([I|Ins], Cats, T2, T3, Classes):-       %% Extract one
    new_categories(Ins, I, Cats, T2, T3, Classes).
new_categories([], I, Cats, T2, T3, Classes):-         %% Only one example
    compute_within(Cats, [I], T2, T3, Classes).
new_categories([I1|Is], I, Cats, T2, T3, Classes):-    %% Two or more
    Ins = [I, I1|Is],
    precluster(Ins, NewIns),
    split(NewIns, _, NewCats),
    find_addable(NewCats, Cats, T3, Addable),
    extract_instances(Ins, Addable, TrueIns),
    add_cats(Addable, TrueIns,  Cats, T2, T3, Classes).

add_cats([], Ins, Cats, T2, T3, Classes):-
    compute_within(Ins, Cats, T2, T3, Classes).
add_cats([C|Cs], Ins, Cats, T2, T3, [Partition|Classes]):-
    my_own_append(Cs, [C|Cats], NewCats),
    my_own_append(Ins, NewCats, Part),
    my_own_select_names(Part, Partition),
    refinement(Ins, NewCats, T2, T3, Classes).

compute_within([C|Cs], Ins, T2, T3, Classes):-
    compute_within(Cs, C, Ins, T2, T3, Classes).
compute_within([], C, Ins, _, _, [Classes]):-        %% Only one cathegory
    my_own_select_names([C|Ins], Classes).
compute_within([Ca1|Cats], Ca2, Ins, T2, T3, Classes):-
    best_cohesion(Ca1, Ca2, Cats, C1, C2, Score),
    compare(C, Score, T3),
    check_end(C, Ins, [Ca1, Ca2|Cats], C1, C2, T2, T3, Classes).

check_end(>, Ins, Cats, C1, C2, T2, T3, [Partition|Classes]):-
    NewWorld = [C3|Cats2],
    combine(C1, C2, C3),
    my_own_select_(C1, Cats, Cats1),
    my_own_select_(C2, Cats1, Cats2),
    my_own_select_names(pair(Ins, NewWorld), Partition),
    refinement(Ins, NewWorld, T2, T3, Classes).
check_end(<, Ins, Cats, _, _, _, _, [Classes]):-
    my_own_select_names(pair(Ins,  Cats), Classes).
check_end(=, Ins, Cats, _, _, _, _, [Classes]):-
    my_own_select_names(pair(Ins,  Cats), Classes).

get_best_pair([In|Ins], [Cat|Cts], BestI, BestC, BestSc):-
    combine(In, Cat, Prv),
    cc(Prv, Cts, Sco), 
    get_best_pair([Cat|Cts],[In|Ins],[],Cat,In,Sco,BestC,BestI,BestSc).

get_best_pair([], _, _, C, I, S, C, I, S). 
get_best_pair([C|Cs], Ins, PCts, PrCat, PrIns, PrSco, BCat, BIns, BSco):-
    my_own_append(Cs, PCts, NCts),
    get_best_pair_ins(Ins,C,NCts,PrCat,PrIns,PrSco,MdCat,MdIns,MdSco),
    get_best_pair(Cs,Ins,[C|PCts],MdCat,MdIns,MdSco,BCat,BIns,BSco).

get_best_pair_ins([], _, _, C, I, S, C, I, S).
get_best_pair_ins([In|Ins], CCat, Cts, PrCat, PrIns, PrSco, BCat, BIns, BSco):-
    combine(In, CCat, NCat),
    cc(NCat, Cts, Sco),
    compare(C, Sco, PrSco),
    my_own_select_tri(C, CCat, In, Sco, PrCat, PrIns, PrSco, MC, MI, MS),
    get_best_pair_ins(Ins, CCat, Cts, MC, MI, MS, BCat, BIns, BSco).
