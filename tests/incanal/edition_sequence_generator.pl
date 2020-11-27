:- module(_, [], [assertions, isomodes, regtypes, nativeprops, hiord, datafacts, fsyntax]).

:- doc(title, "Generator of sequence editions for sets of modules").

:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "

This module generates sequences of states sets, i.e., having present
their elements or not, (represented as lists).

To specify the configuration of the sets a list must be
created of the form [NameSet1-NObjects1, NameSet2-NObjects2 ...]. Each
step of the sequence is composed of an atom p(Name, List), with
the List of length NObjects of the set.

@section{Example}

This is an example to create a sequence in which elements are added 1
by 1 until the sets have all their elements present. In this case we
are using the number 1 to mark that an element is present:

```
?- gen_edit_sequence([m3-1, m2-1, m1-2], cfg(4, 4, 1, 1,0,4), gen_num_sequence, S).

S =[[p(m3,[1]),p(m2,[_]),p(m1,[_,_])],
    [p(m3,[1]),p(m2,[1]),p(m1,[_,_])],
    [p(m3,[1]),p(m2,[1]),p(m1,[1,_])],
    [p(m3,[1]),p(m2,[1]),p(m1,[1,1])]]
?

yes
?-
```

An example to get a sequence in which elements are added 2 by 2 and randomly:
```
?- gen_edit_sequence([set1-3, set2-1, set3-2], cfg(6, 3, 2, 1, 0, 3), gen_random_sorted_sequence, S).

S = [[p(set1,[_,_,_]),p(set2,[_]),p(set3,[_,_])],
     [p(set1,[_,1,_]),p(set2,[1]),p(set3,[_,_])],
     [p(set1,[_,1,_]),p(set2,[1]),p(set3,[1,1])],
     [p(set1,[1,1,1]),p(set2,[1]),p(set3,[1,1])]] ? 

yes
?- 
```
").

:- use_module(library(hiordlib)).
:- use_module(library(lists), [member/2, length/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(aggregates)).
:- use_module(library(random), [random/3]).

:- use_module(config_db).

:- data sets/1.
:- data edit_config/2.

:- export(gen_edit_sequence/4).
:- meta_predicate gen_edit_sequence(?,?,pred(4),?).
:- pred gen_edit_sequence(+Mods, +SeqConfig, +SeqGenerator, -Seq)
   + (not_fails, is_det).
gen_edit_sequence(Mods,SeqConfig, SeqGenerator,[State1|Seq]) :-
    SeqConfig = cfg(NCls, NSteps, NEdits, EditType,PreSkip,MaxSteps),
    init_gen_sequence(EditType, Mods, State0, StateI),
    ( PreSkip \= 0 ->
        next_state(StateI, NSteps, PreSkip, SeqGenerator),
        State1 = StateI,
        copy_term(StateI,StateNI)
    ;
        State1 = State0, StateNI = StateI
    ),
    gen_state_seq(0, StateNI, NCls, NSteps, NEdits, MaxSteps, SeqGenerator, Seq).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, "Sequence generators").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export(gen_random_sorted_sequence/4).
:- export(gen_num_sequence/4).
:- export(gen_pred_num/4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, "Generate initial state").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_gen_sequence(EditT, Mods, IniState, State) :-
    ( get_edit_mode(predicate) ->
        init_gen_sequence_preds(EditT, Mods, IniState, State)
    ;
        init_gen_sequence_cl(EditT, Mods, IniState, State)
    ).

% ignoring predicates
init_gen_sequence_cl(EditT, Mods, IniState, State) :-
    set_fact(edit_config(atom,EditT)),
    gen_empty_state(Mods, State),
    copy_term(State, IniState).

gen_empty_state([], []).
gen_empty_state([Mod-NClauses|Mods], [p(Mod, List)|IniState]) :-
    length(List, NClauses),
    gen_empty_state(Mods, IniState).

% predicate-aware
init_gen_sequence_preds(EditT, Mods, IniState, State) :-
    set_fact(edit_config(atom,EditT)),
    gen_empty_state_preds(Mods, State),
    copy_term(State, IniState).

gen_empty_state_preds([], []).
gen_empty_state_preds([Mod-PredsSum|Mods], [p(Mod, List)|IniState]) :-
    findall(P/A-L, (member(P/A-N, PredsSum), length(L, N)), List),
    gen_empty_state_preds(Mods, IniState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, "Generate sequence").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate gen_state_seq(+,?,+,+,+,pred(4),?).
gen_state_seq(NSteps, _, _, NSteps, _, _,  _, []) :- !.
gen_state_seq(NSteps, _, _, _, _, NSteps, _, []) :- !. % NSteps = MaxSteps
gen_state_seq(ISteps, State, NClauses, NSteps, NEdits, MaxSteps, SeqGenerator,[S1|Seq]) :-
    ISteps < NSteps, !,
    next_state(State, NClauses, NEdits, SeqGenerator),
    copy_term(State, S1),
    I1 is ISteps + 1,
    NCl2 is NClauses - NEdits,
    gen_state_seq(I1, State, NCl2, NSteps, NEdits, MaxSteps, SeqGenerator, Seq).

:- meta_predicate next_state(+,+,+,pred(4)).
:- pred next_state(Mods, NClauses, Adds, SeqGenerator)
   : edit_state * int * int * term
   #"@var{SeqGenerator} generates a sequence of numbers of the length of the
    number of clauses or predicates to be added in one step. The numbers are
    used to unify elements at the nth position of the state with a value of
    @tt{edit_config(atom,V)}, skipping those already unified.".
next_state(Mods, NCl, Adds, _SeqGenerator) :-
    NCl - Adds =< 0, !, % last state
    fill_state(Mods).
next_state(Mods, NCl, Adds, SeqGenerator) :-
    SeqGenerator(Adds, 0, ~(NCl - 1), Seq),
    ( get_file_config(edition_mode(predicate)) ->
        update_state_set_preds(Mods, Seq)
    ;
        update_state_set_first(Mods, Seq)
    ).

update_state_set_first(Mods, Seq) :- % fill module by module
    update_state_set_first_(Mods, 0, Seq).

update_state_set_first_([], _, []) :- !.
update_state_set_first_(_, _, []) :- !. % no more additions
update_state_set_first_([p(_, List)|State], I, Ns) :-
    update_cl_state(List, I, Ns, NewI, RestNs),
    update_state_set_first_(State, NewI, RestNs).

update_cl_state(List, InitI, Ns, NewI, RestNs) :-
    inst_with_data(List, InitI, Ns, ~edit_config(atom), NewI, RestNs).

update_state_uniform_set(Mods, Seq) :-
    update_state_uniform_set_(Mods, 0, Mods, Seq).

update_state_uniform_set_(_, _, _, []) :- !.
update_state_uniform_set_([], I, States, Ns) :- !,
    update_state_uniform_set_(States, I, States, Ns).
update_state_uniform_set_([p(_, List)|State], I, States, [N|Ns]) :-
    update_cl_state(List, 0, [0], _, RestNs),
    ( RestNs = [_] ->
        NextNs = [N|Ns]
    ;
        NextNs = Ns
    ),
    update_state_uniform_set_(State, I, States, NextNs).

inst_with_data([], I, Ns, _, I, Ns).
inst_with_data([Ele|L], I, LPos, Data, NewI, NewNs) :- % Do not count already added
                                                   % clauses
    Ele == Data, !,
    inst_with_data(L, I, LPos, Data, NewI, NewNs).
inst_with_data([Ele|L], I, [I|LPos], Data, NewI, NewNs) :-  !, % Insert
    ( Ele == Data ->
        I1 = I
    ;
        Ele = Data,
        I1 is I + 1
    ),
    inst_with_data(L, I1, LPos, Data, NewI, NewNs).
inst_with_data([_|L], I, LPos, Data, NewI, NewNs) :-        % Skip
    I1 is I + 1,
    inst_with_data(L, I1, LPos, Data, NewI, NewNs).

fill_with_data([], _).
fill_with_data([Data|Xs], Data) :- !,
    fill_with_data(Xs, Data).
fill_with_data([_|Xs], Data) :-
    fill_with_data(Xs, Data).

fill_state([]).
fill_state([p(_, List)|IniState]) :-
    fill_list(List, ~edit_config(atom)),
    fill_state(IniState).

fill_list([], _).
fill_list([A|As], Data) :-
    ( ( nonvar(A), A = _/_-L) ->
        fill_list(L, Data)
    ;
        A = Data
    ),
    fill_list(As, Data).

:- regtype edit_state/1.
edit_state([]).
edit_state([p(ModId, List)|IniState]) :-
    atom(ModId),
    list(List),
    edit_state(IniState).

gen_num_sequence(NNumbers, Low, _, Seq) :-
    gen_num_sequence_(NNumbers, 0, Low, Seq).

gen_num_sequence_(1, Acc, Low, [Max]) :- !,
    Max is Low + Acc.
gen_num_sequence_(NNumbers, Acc, Low, [N|Seq]) :-
    N1 is NNumbers - 1,
    A1 is Acc + 1,
    N is Low + Acc,
    gen_num_sequence_(N1, A1, Low, Seq).

% ------------------------------------------------------------
:- doc(subsection, "Predicate-aware states").
% ------------------------------------------------------------

:- data editing_mod/1.
:- data finished_mod/1.

% modules are sorted by priority to contain clauses
% add predicates 1 by 1

update_state_set_preds([], _) :- !.
update_state_set_preds(_, 0) :- !.
update_state_set_preds([p(_Mod,Preds)|Mods], N0) :-
    update_state_set_preds_mod(Preds,N0,N),
    update_state_set_preds(Mods,N).

update_state_set_preds_mod(_,0,0) :- !.
update_state_set_preds_mod([],N,N) :- !.
update_state_set_preds_mod([_P/_A-LCls|Ps],N0,N) :-
    edit_config(atom,EA),
    ( maplist('=='(EA), LCls) -> % pred already marked
        N1 = N0
    ;
        maplist('='(EA), LCls), % fill all clauses of the predicate
        N1 is N0 - 1
    ),
    update_state_set_preds_mod(Ps,N1,N).

get_prev_mod([p(_M1-P),p(M2,_)|_], M2, p(M2,P)) :- !.
get_prev_mod([_|Ms], M, R) :-
    get_prev_mod(Ms, M, R).

get_next_mod([p(M1,P),p(M2,_)|_], M1, p(M2,P)) :- !.
get_next_mod([_|Ms], M, R) :-
    get_next_mod(Ms, M, R).

get_mod([p(M,P)|_], M, p(M,P)) :- !.
get_mod([_|Ms], M, R) :-
    get_mod(Ms, M, R).

is_empty_mod(p(_M,P)) :-
    edit_config(atom,EA),
    maplist(check_empty_mod(EA),P).

check_empty_mod(EA, _P/_A-L) :-
    maplist('\=='(EA), L).

is_full_mod(p(_M,P)) :-
    edit_config(atom,EA),
    maplist(check_full_mod(EA),P).

check_full_mod(EA, _P/_A-L) :-
    maplist('=='(EA), L).

% TODO: check this
priority_mods_full(Ms, PM) :-
    \+ ( (member(M,PM), get_mod(Ms,M,ModSt)), % forall M in PM
         \+ is_full_mod(ModSt) ).

% TODO: check this
non_priority_mods_empty(Ms, PM) :-
    \+ ( (member(M,PM), get_mod(Ms,M,ModSt)), % forall M in PM
         \+ is_full_mod(ModSt) ).

decide_rand(_A,add,M,State) :-
    % check that the rest of modules are full
    \+ ( (member(p(M0,ModSt),State), M \= M0), % forall M in PM
         \+ is_full_mod(ModSt) ).
decide_rand(_A,del,M,State) :-
    % check that the rest of modules are empty
    \+ ( (member(p(M0,ModSt),State), M \= M0), % forall M in PM
         \+ is_empty_mod(ModSt) ).
decide_rand(A,_,_M,_State) :-
    F =.. [A|N],
    get_file_config(F), % instantiates N
    random(0,1,N0), N0 < N.

get_modif_mod_add(State,M,NM) :-
    get_prev_mod(State,M,NM), % there may not be a 'prev' mod if M is the first
    \+ is_full_mod(NM).
get_modif_mod_add(State,M,NM) :-
    get_next_mod(State,M,NM).

get_modif_mod_del(State,M,NM) :-
    get_next_mod(State,M,NM), % there may not be a 'next' mod if M is the last
    \+ is_empty_mod(NM).
get_modif_mod_del(State,M,NM) :-
    get_prev_mod(State,M,NM).

% TODO: include add/del_probability

% check before if add or del is active!!
update_state_set_preds_rand(StateI, StateO, M, NPreds) :-
    % decide to change mod for adding
    ( decide_rand(module_permanence,add,M,StateI) ->
        get_modif_mod_add(StateI,M,NM)
    ;   NM = M
    ),
    add_preds(StateI, NM, NPreds),
    % decide to change mod for deleting
    ( decide_rand(module_permanence,del,M,StateI) ->
        get_modif_mod_del(StateI,M,NM)
    ; NM = M
    ),
    del_preds(StateI, StateO, NM, NPreds).

% 'activate' NPreds in the current module `Mod`
add_preds(State, Mod, NPreds) :-
    update_state_set_preds_mod(~get_mod(State,Mod),NPreds,P0), % update current module
    ( P0 \= 0, get_prev_mod(State,Mod,NextM) -> % more predicates need to be written
        % update prev mod (priority)
        update_state_set_preds_mod(~get_mod(State,NextM),P0,P1)
    ; P1 = P0 ),
    ( P1 \= 0 ->  % more predicates need to be written, continue in order
        update_state_set_preds(State,P1)
    ; true ).

% 'deactivate' NPreds in the current module `Mod`
del_preds(State0, State, Mod, NPreds) :-
    update_state_unset_preds(State0,State1,Mod,NPreds,P0), % update current module
    ( (P0 \= 0, get_next_mod(State,Mod,NextM)) -> % more predicates need to be removed
        update_state_unset_preds(State1,State2,NextM,P0,P1)  % update prev mod (priority)
    ; P1 = P0, State2 = State1),
    ( P1 \= 0 ->  % if more predicates need to be written, continue in order
        update_state_unset_rest_preds(State2,State,P1,_)
    ; true ).

% update_state_unset_preds_mod(OldState,NewState,Mod,NPreds,PendingN).
update_state_unset_preds([], [], _, N, N) :- !.
update_state_unset_preds([p(Mod,Preds0)|Ms], [p(Mod,Preds)|Ms],Mod, N0, N) :- !,
    update_state_unset_preds_mod(Preds0,Preds,N0,N).
update_state_unset_preds([M|Ms0], [M|Ms], Mod, N, N0) :-
    update_state_unset_preds(Ms0, Ms, Mod, N, N0).


update_state_unset_rest_preds([],[],N,N) :- !.
update_state_unset_rest_preds(St,St,0,0) :- !.
update_state_unset_rest_preds([p(Mod,Preds0)|Ms0],[p(Mod,Preds)|Ms],N0,N) :-
    update_state_unset_rest_preds(Ms0,Ms,N0,N1),
    update_state_unset_preds_mod(Preds0,Preds,N1,N).

update_state_unset_preds_mod(St,St,0,0) :- !. % not necessary?
update_state_unset_preds_mod([],[],N,N) :- !.
update_state_unset_preds_mod([_P/A-LCls|Ps],[_P/A-L|NPs],N0,N) :-
    update_state_unset_preds_mod(Ps,NPs,N0,N1),
    ( maplist('=='(~edit_config(atom)), LCls) -> % pred marked, unset
        length(A,L),
        N is N1 - 1
    ;
        L = LCls, N = N1
    ).

gen_pred_num(NNumbers, _, _, NNumbers).

% ------------------------------------------------------------
:- doc(section, "Generate random sequence").
% ------------------------------------------------------------

% TODO: move this predicate somewhere else?
% TODO: very inefficient when NNumbers is close to Up - Low.
:- pred gen_random_sorted_sequence(NNumbers, Low, Up, Seq) #"Generates a
    random sequence of different numbers with length @var{NNumbers} .".
gen_random_sorted_sequence(NNumbers, Low, Up, SSeq) :-
    Range is Up - Low,
    Range >= NNumbers,
    gen_random_sequence_(NNumbers, Low, Up, [], Seq),
    sort(Seq, SSeq).

gen_random_sequence_(0, _, _, Seq, Seq) :- !.
gen_random_sequence_(N, Low, Up, AccSeq, Seq) :-
    backtrack_random(Low, Up, Number), % get a random number
                                       % until it was not present
    \+ member(Number, AccSeq), !,
    gen_random_sequence_(~(N - 1), Low, Up, [Number|AccSeq], Seq).

backtrack_random(Low, Up, Number) :-
    random(Low, Up, Number).
backtrack_random(Low, Up, Number) :-
    backtrack_random(Low, Up, Number).
