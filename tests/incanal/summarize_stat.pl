:- module(summarize_stat, [main/1, summarize_dir/2], [assertions, datafacts]).

:- doc(title, "Summarize the statistics of a modular analysis execution").

:- doc(module, "This module summarizes the statistics generated for
each iteration during the execution of an analysis performed with
@tt{intermod}

").

:- doc(author, "Isabel Garcia-Contreras").

% TODO: rename by accumulate stats

:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(read), [read/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(format), [format/3]).
:- use_module(library(system), [directory_files/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).

:- use_module(ciaopp(analysis_stats), [get_stat/2]).

:- export(all_tmp_stat/2).
:- data all_tmp_stat/2.
:- data tmp_stat/2.

:- pred main(Args) : list(Args).
main([X]) :- !,
    ( summarize_dir(X, user) -> true
    ; display('Not a statistics directory\n')
    ).
main(_) :-
    display('Usage ./summarize_stat <stat_dir>'), nl.

summarize_dir(Dir, View) :-
    check_stats_dir(Dir, _),
    retractall_fact(all_tmp_stat(_, _)),
    stat_directory_files_paths(Dir, AllFs),
    %AllFs = [_|Fs],
    Fs = AllFs,
    ( % failure-driven loop
      member(F, Fs),
        summarize_file(F),
        fail
    ;
        write_total(View, [load_time, comp_diff, restore, proc_diff, proc_assrts, preproc, upd_clauses, inc_time, fixp, genreg, savereg,prog_mem,it])
    ).

:- export(check_stats_dir/2).
check_stats_dir(Dir, StatsFilePath) :-
    path_split(Dir, _, Base),
    atom_codes(Base, "stats"),
    path_concat(Dir, 'stats.pl', StatsFilePath).

:- export(summarize_file/1).
summarize_file(File) :-
    retractall_fact(tmp_stat(_, _)),
    open(File, read, S),
    read_stats_file(S),
    close(S),
    add_to_total_display_sum.

:- export(read_stats_file/1).
read_stats_file(F):-
    repeat,
    read(F,X),
    (X = end_of_file, !
        ; process(X),
          fail).

process(X) :-
    X = step(_N, _Mod, Load, Ana, GenReg, SaveReg, Memory),
    assertz_fact(tmp_stat(it, 1)),
    process_(Load, Ana, GenReg, SaveReg, Memory).

% process incremental time
process_(Load, Ana, GenReg, SaveReg, Memory) :-
    get_stat(Load, comp_diff(A)), !,
    assertz_fact(tmp_stat(comp_diff, A)),
    get_stat(Load, time(B, _)),
    assertz_fact(tmp_stat(load_time, B)),
    get_stat(Load, restore(Rest)),
    assertz_fact(tmp_stat(restore, Rest)),
    get_stat(Ana, proc_diff(C)),
    assertz_fact(tmp_stat(proc_diff, C)),
    get_stat(Ana, proc_assrts(C1)),
    assertz_fact(tmp_stat(proc_assrts, C1)),
    get_stat(Ana, td_add(D)),
    ( get_stat(Ana, fixp(FixpT)) -> true  ; FixpT = 0),
    assertz_fact(tmp_stat(fixp, FixpT)),
    ( get_stat(Ana, preproc(PreProcT)) -> true  ; PreProcT = 0),
      assertz_fact(tmp_stat(preproc, PreProcT)),
    ( get_stat(Ana, upd_clauses(UpdCT)) -> true  ; UpdCT = 0),
    assertz_fact(tmp_stat(udp_clauses, UpdCT)),
    ( get_stat(Ana, td_delete(E)), ! ; get_stat(Ana, bu_delete(E))),
     F is D + E,
    get_stat(Ana, upd_lat(UpdT)),
    AnaT is F + UpdT,
    assertz_fact(tmp_stat(inc_time, AnaT)),
    process_common_stats(Load, Ana, GenReg, SaveReg, Memory).
process_(Load, Ana, GenReg, SaveReg, Memory) :-
    ( get_stat(Load, time(LT, _)) -> true
    ; LT = Load
    ),
    assertz_fact(tmp_stat(load_time, LT)),
    ( get_stat(Ana, time(_, [(prep,PreT),(ana,AT)])) -> true
    ; AT = Ana, PreT = 0
    ),
    assertz_fact(tmp_stat(preproc, PreT)),
    assertz_fact(tmp_stat(fixp, AT)),
    process_common_stats(Load, Ana, GenReg, SaveReg, Memory).

process_common_stats(Load, Ana, GenReg, SaveReg, Memory) :-
    ( member(A, [genreg, savereg]),
      add_stat_to_sum(A, A, Load, Ana, GenReg, SaveReg, Memory), fail
    ; true
    ),
    Memory = memory(_, MStats),
    get_stat(MStats, (program,Mem, _)),
    assertz_fact(tmp_stat(prog_mem, Mem)).

add_stat_to_sum(What, StatDBId, Load, Ana, GenReg, SaveReg, Memory) :-
    get_stat_db(StatDBId, Load, Ana, GenReg, SaveReg, Memory, StatDB),
    ( get_stat(StatDB, time(Stat, _)) ->
        true
    ;   Stat = StatDB
    ),
    assertz_fact(tmp_stat(What, Stat)).

get_stat_db(load, Load, _, _, _, _, Load).
get_stat_db(analyze, _, Ana, _, _, _, Ana).
get_stat_db(genreg, _, _, GenReg, _, _, GenReg).
get_stat_db(savereg, _, _, _, SaveReg, _, SaveReg).
get_stat_db(memory, _, _, _, _, Memory, Memory).

add_to_total_display_sum :-
    ( % failure-driven loop
        member(X, [comp_diff, load_time, proc_diff, proc_assrts, restore, preproc, inc_time, fixp, genreg, savereg, it]),
        add_to_total(X, _),
        fail
    ; true).
%       max_to_total(prog_mem, _). % keeping the maximum amount of memory each iteration

add_to_total(Stat, X) :-
    sum_stat(Stat, X),
    get_all_tmp_stat(Stat, B),
    C is B + X,
    assertz_fact(all_tmp_stat(Stat, C)).

max_to_total(Stat, X) :-
    max_stat(Stat, X),
    get_all_tmp_stat(Stat, Max),
    ( X > Max ->
      assertz_fact(all_tmp_stat(Stat, X))
    ; assertz_fact(all_tmp_stat(Stat, Max))).

get_all_tmp_stat(A, B) :-
    retract_fact(all_tmp_stat(A, B)), !.
get_all_tmp_stat(_, 0).

sum_stat(What, Sum) :-
    findall(T, tmp_stat(What, T), Ts),
    add_all(Ts, 0, Sum).

max_stat(What, Max) :-
    findall(T, tmp_stat(What, T), Ts),
    max_all(Ts, 0, Max).

max_all([], Max, Max).
max_all([N|Ns], Prev, Max) :-
    N > Prev, !,
    max_all(Ns, N, Max).
max_all([_|Ns], Prev, Max) :-
    max_all(Ns, Prev, Max).

add_all([], Acc, Acc).
add_all([N|Ns], Acc, S) :-
    Sum is Acc + N,
    add_all(Ns, Sum, S).

:- export(write_total/2).
write_total(S, Data) :- !,
    Data = [First|Rest],
    all_tmp_stat(First, CompDiff),
    format(S, ' ~3f', [CompDiff]),
    ( % failure-driven loop
      member(Stat, Rest),
        format_stat(Stat, S),
        fail
    ;   true),
    format(S, ').\n', []).

format_stat(What,S) :-
    all_tmp_stat(What, Stat),
    format(S,', ~3f', [Stat]).

:- export(stat_directory_files/2).
% TODO: IG: move?
stat_directory_files(Dir, Files_s) :-
    directory_files(Dir, Fs),
    filter_state_dirs(Fs, Files),
    sort(Files, Files_s).

filter_state_dirs([], []).
filter_state_dirs([F|Fs], FFs) :-  % I do not want files beginning with dot
    filter_file(F), !,
    filter_state_dirs(Fs, FFs).
filter_state_dirs([F|Fs], [F|FFs]) :-
    filter_state_dirs(Fs, FFs).

filter_file(F) :-
    atom_codes(F,X),
    filter(X).

filter([0'.|_]).
filter(L) :- % take only files with extension '.stat'
    ( append(_, ".stat", L) ->
      fail
    ; true ).

:- export(stat_directory_files_paths/2).
stat_directory_files_paths(Dir, FilePaths) :-
    stat_directory_files(Dir, Files),
    path_concat_list(Files, Dir, FilePaths).

path_concat_list([], _, []).
path_concat_list([F|Fs], Dir, [P|Ps]) :-
    path_concat(Dir, F, P),
    path_concat_list(Fs, Dir, Ps).
