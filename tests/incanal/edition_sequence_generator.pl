:- module(_, [], [assertions, regtypes, hiord, datafacts]).

:- use_module(engine(stream_basic), [stream/1]).
:- use_module(engine(io_basic)).
:- use_module(library(lists), [member/2, length/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(random), [random/3]).
:- use_module(library(format), [format/3]).

:- doc(author, "Isabel Garcia-Contreras").

:- doc(title, "Generator of sequence editions for sets of modules").

:- doc(module, "

This module generates sequences of states sets, i.e., having present
their elements or not, (represented as lists).

To specify the configuration of the sets a list must be
created of the form [NameSet1-NObjects1, NameSet2-NObjects2 ...]. Each
step of the sequence is composed of an atom present(Name, List), with
the List of length NObjects of the set.

@section{Example}

This is an example to create a sequence in which elements are added 1
by 1 until the sets have all their elements present. In this case we
are using the number 1 to mark that an element is present:

```
?- generate_edit_sequence_mods([m3-1, m2-1, m1-2], 4, 4, 1, 1, S).

S =[[present(m3,[1]),present(m2,[_]),present(m1,[_,_])],
    [present(m3,[1]),present(m2,[1]),present(m1,[_,_])],
    [present(m3,[1]),present(m2,[1]),present(m1,[1,_])],
    [present(m3,[1]),present(m2,[1]),present(m1,[1,1])]]
?

yes
?-
```

An example to get a sequence in which elements are added 2 by 2 and randomly:
```
?- generate_random_edit_sequence([set1-3, set2-1, set3-2], 6, 3, 2, 1, S).

S = [[present(set1,[_,_,_]),present(set2,[_]),present(set3,[_,_])],
     [present(set1,[_,1,_]),present(set2,[1]),present(set3,[_,_])],
     [present(set1,[_,1,_]),present(set2,[1]),present(set3,[1,1])],
     [present(set1,[1,1,1]),present(set2,[1]),present(set3,[1,1])]] ? 

yes
?- 
```
").

:- data edit_atom/1.
:- data sets/1.

:- export(generate_random_edit_sequence/6).
:- pred generate_random_edit_sequence(Mods, NCls, NSteps, NEdits, EditType, Seq)
    #"Generates in @var{Seq} a sequence @var{NSteps} steps of
     statuses of predicate editions for @var{Mods} with a
     difference of @var{NEdits} of type @var{EditType}.".
generate_random_edit_sequence(Mods, NCls, NSteps, NEdits, EditT, [IniState|Seq]) :-
    init_gen_sequence(EditT, Mods, IniState, State),
    gen_state_seq(0, State, NCls, NSteps, NEdits, gen_random_sorted_sequence, Seq).

:- export(generate_edit_sequence_mods/6).
generate_edit_sequence_mods(Mods, NCls, NSteps, NEdits, EditT, [IniState|Seq]) :-
    init_gen_sequence(EditT, Mods, IniState, State),
    gen_state_seq(0, State, NCls, NSteps, NEdits, generate_num_sequence, Seq).

:- export(generate_edit_sequence_uniform_mods/6).
generate_edit_sequence_uniform_mods(Mods, NCls, NSteps, NEdits, EditT, [IniState|Seq]) :-
    init_gen_sequence(EditT, Mods, IniState, State),
    set_fact(sets(Mods)),
    gen_state_seq(0, State, NCls, NSteps, NEdits, generate_num_sequence, Seq).

init_gen_sequence(EditT, Mods, IniState, State) :-
    set_fact(edit_atom(EditT)),
    gen_empty_state(Mods, State),
    copy_term(State, IniState).

:- meta_predicate gen_state_seq(+,?,+,+,+,pred(4),?).
gen_state_seq(NSteps, _, _, NSteps, _, _, []) :- !.
gen_state_seq(ISteps, State, NClauses, NSteps, NEdits, SeqGenerator,[S1|Seq]) :-
    ISteps < NSteps, !,
    next_state(State, NClauses, NEdits, SeqGenerator),
    copy_term(State, S1),
    I1 is ISteps + 1,
    NCl2 is NClauses - NEdits,
    gen_state_seq(I1, State, NCl2, NSteps, NEdits, SeqGenerator, Seq).

gen_empty_state([], []).
gen_empty_state([Mod-NClauses|Mods], [present(Mod, List)|IniState]) :-
    length(List, NClauses),
    gen_empty_state(Mods, IniState).

:- meta_predicate next_state(+,+,+,pred(4)).
:- pred next_state(Mods, NClauses, Adds, SeqGenerator) : edit_state * int * int * term.
next_state(Mods, NClauses, Adds, _SeqGenerator) :-
    Remaining is NClauses - Adds,
    Remaining =< 0, !, % last state
    fill_state(Mods).
next_state(Mods, NClauses, Adds, SeqGenerator) :-
    N1 is NClauses - 1,
    SeqGenerator(Adds, 0, N1, Seq),
    ( sets(_) ->
        update_state_uniform_set(Mods, Seq)
    ;
        update_state_set_first(Mods, Seq)
    ).

update_state_set_first(Mods, Seq) :- % fill module by module
    update_state_set_first_(Mods, 0, Seq).

update_state_set_first_([], _, []) :- !.
update_state_set_first_(_, _, []) :- !. % no more additions
update_state_set_first_([present(_, List)|State], I, Ns) :-
    update_pred_state(List, I, Ns, NewI, RestNs),
    update_state_set_first_(State, NewI, RestNs).

update_pred_state(List, InitI, Ns, NewI, RestNs) :-
    edit_atom(Data),
    inst_with_data(List, InitI, Ns, Data, NewI, RestNs).

update_state_uniform_set(Mods, Seq) :-
    update_state_uniform_set_(Mods, 0, Mods, Seq),
    display(update_state_uniform_set(Mods, Seq)), nl.

update_state_uniform_set_(_, _, _, []) :- !.
update_state_uniform_set_([], I, States, Ns) :- !,
    update_state_uniform_set_(States, I, States, Ns).
update_state_uniform_set_([present(_, List)|State], I, States, [N|Ns]) :-
    update_pred_state(List, 0, [0], _, RestNs),
    ( RestNs = [_] ->
        NextNs = [N|Ns]
    ;
        NextNs = Ns
    ),
    update_state_uniform_set_(State, I, States, NextNs).

inst_with_data([], I, Ns, _, I, Ns).
inst_with_data([Ele|L], I, LPos, Data, NewI, NewNs) :- %Do not count already added
                                                   %clauses
    Ele == Data, !,
    inst_with_data(L, I, LPos, Data, NewI, NewNs).
inst_with_data([Ele|L], I, [I|LPos], Data, NewI, NewNs) :-  !,% Insert
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

display_edit_sequence([], _).
display_edit_sequence([S|Ss], Stream) :-
    display_edit_state(S, Stream), nl(Stream),
    display_edit_sequence(Ss, Stream).

fill_state([]).
fill_state([present(_, List)|IniState]) :-
    edit_atom(Data),
    fill_list(List, Data),
    fill_state(IniState).

fill_list([], _).
fill_list([A|As], Data) :-
    A = Data,
    fill_list(As, Data).

:- pred display_edit_state(State, Stream) : edit_state * stream.
display_edit_state([], Stream) :-
    nl(Stream).
display_edit_state([present(Mod, List)|IniState], Stream) :-
    format(Stream, '~w - ~w~n', [Mod, List]),
    display_edit_state(IniState, Stream).

:- regtype edit_state/1.
edit_state([]).
edit_state([present(ModId, List)|IniState]) :-
    atom(ModId),
    list(List),
    edit_state(IniState).

generate_num_sequence(NNumbers, Low, _, Seq) :-
    generate_num_sequence_(NNumbers, 0, Low, Seq).

generate_num_sequence_(1, Acc, Low, [Max]) :- !,
    Max is Low + Acc.
generate_num_sequence_(NNumbers, Acc, Low, [N|Seq]) :-
    N1 is NNumbers - 1,
    A1 is Acc + 1,
    N is Low + Acc,
    generate_num_sequence_(N1, A1, Low, Seq).

% ------------------------------------------------------------
:- doc(section, "Generate random sequence").

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
    N1 is N - 1,
    gen_random_sequence_(N1, Low, Up, [Number|AccSeq], Seq).

backtrack_random(Low, Up, Number) :-
    random(Low, Up, Number).
backtrack_random(Low, Up, Number) :-
    backtrack_random(Low, Up, Number).
