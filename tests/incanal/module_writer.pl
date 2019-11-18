:- module(module_writer, [write_dir_state/4, write_dir_state_sequence/3],
    [assertions, datafacts,hiord]).

:- use_module(engine(stream_basic)).
:- use_module(engine(internals), [itf_filename/2]).
:- use_module(library(write), [portray_clause/2]).
:- use_module(library(aggregates)).
:- use_module(library(pathnames), [path_concat/3, path_get_relative/3]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(system), [make_directory/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(terms)).

:- use_module(ciaopp_tests(incanal/git_wrapper)).

:- use_module(ciaopp_tests(incanal/naive_reader)).
:- use_module(ciaopp_tests(incanal/incanal_intermod_bench_driver), [monolithic/0]).

:- doc(title, "Modular program printer").

:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "This module writes in a directory a program containing
only the clauses specified by a specific state (generated by
edition_sequence_generator).").

:- pred mod_prev_state(Mod, State) #"Previous state of the module (to
    avoid rewriting).".
:- data mod_prev_state/2.

:- data modified/1. % This modified is needed to later remove itf files

% IG: The removing of the itf files is needed because the file system
% may not be accurate enough to detect changes in files by checking
% their timestamp.

:- pred write_dir_state_sequence(+Seq, +DirType, +Dir) #"Writes in directory
    @var{Dir} a directory for each of the steps in sequence @var{Seq}.".
write_dir_state_sequence(Seq,states,Dir) :- !,
    copy_list_directories(Seq,Dir).
write_dir_state_sequence(Seq,DirType,Dir) :-
    write_dir_state_sequence_(Seq, DirType,Dir, 0),
    retractall_fact(mod_prev_state(_, _)).

copy_list_directories([],_).
copy_list_directories([S|Seq],Dir) :-
    process_call(path(cp), ['-r', S, Dir], []),
    copy_list_directories(Seq,Dir).

write_dir_state_sequence_([], _, _, _).
write_dir_state_sequence_([St|Seq], DirType, Dir, N) :-
    N1 is N + 1,
    atom_number(AN, N),
    atom_concat(state_, AN, D),
    path_concat(Dir, D, LocalDir),
    make_directory(LocalDir),
    write_dir_state(St, DirType, LocalDir,git_checkout_copy),
    write_dir_state_sequence_(Seq, DirType, Dir, N1).

:- meta_predicate write_dir_state(?,?,?,pred(3)).
:- pred write_dir_state(+State, +DirType, +DstDir,CheckoutPred) #"Writes @var{State} of clauses
    in modules in directory @var{DstDir}.".
write_dir_state(St, states, Dir, _) :- !,
    atom_concat(St,'/',AllSt),
    process_call(path(cp), ['-r', AllSt, Dir], []),
    % remove itfs
    ( % failure-driven loop
      current_file_find([proj(compilable), file_p-match(name, glob('*.pl'))], St, File),
        path_get_relative(St,File,RelP),
        remove_itf_file(Dir,RelP),
        fail
    ;
        true
    ).
write_dir_state(State, manual, DstDir,_) :- !,
    retractall_fact(modified(_)),
    write_dir_state_(State, DstDir),
    remove_modified_aux_files(DstDir).
write_dir_state(State, git, DstDir,Checkout) :-
    Checkout(incanal_git, DstDir, State),
    git_diff_previous(incanal_git,State,Files),
    ( % failure-driven loop
      member(F,Files),
        remove_itf_file(DstDir, F),
        fail
    ;
        true
    ).

remove_before_state([P|T],[P|T]) :-
    state_dir(P), !.
remove_before_state([_|T],R) :-
    remove_before_state(T,R).

state_dir(DirName) :-
    atom_codes(DirName,L),
    append("state_", _, L), !.

write_dir_state_([], _).
write_dir_state_([present(ModName, ClList)|IniState], DstDir) :-
    mod_full_path(ModName, DstDir, ModPath),
    write_mod_state(ModName, ClList, ModPath),
    write_dir_state_(IniState, DstDir).

mod_full_path(ModName, Dir, ModPath) :-
    path_concat(Dir, ModName, A),
    atom_concat(A, '.pl', ModPath).

write_mod_state(Mod, ClList, _) :- % this file did not change
    current_fact(mod_prev_state(Mod, PrevClList)),
    PrevClList == ClList, !.
write_mod_state(Mod, ClList, ModPath) :-
    open(ModPath, write, ModS),
    retractall_fact(written_pred(_, _)),
    mod_replace_state(Mod, ClList),
    write_mod_header(Mod, ModS),
    write_failures(Mod, ModS), % IG for all clauses, always an
                               % initial fail is written
    write_state_cls(ClList, Mod, ModS),
    assertz_fact(modified(Mod)),
    close(ModS).

mod_replace_state(Mod, ClList) :-
    retractall_fact(mod_prev_state(Mod, _)),
    assertz_fact(mod_prev_state(Mod, ClList)).

write_mod_header(Mod, ModS) :-
    module_clause(module, _, _, Mod, _, Cl),
    write_cl(module, Cl, ModS),
    fail.
write_mod_header(Mod, ModS) :-
    module_clause(directive, _, _, Mod, _, Cl),
    write_cl(directive, Cl, ModS),
    fail.
write_mod_header(Mod, ModS) :-   % Write here entries also
    module_clause(assertion, _, _, Mod, _, Cl),
    write_cl(assertion, Cl, ModS),
    fail.
write_mod_header(_, _).

:- data written_pred/2.

add_written(P, A) :-
    ( written_pred(P, A) ->
      true
    ; assertz_fact(written_pred(P, A))).

write_failures(Mod, S) :-
    module_clause(clause, P, A, Mod, _, _),
    \+ written_pred(P, A),
    write_failure((P, A), S),
    add_written(P, A),
    fail.
write_failures(_, _).

write_failure((P, A), S) :-
    functor(F, P, A),
    portray_clause(S, ':-'(F, fail)).

write_state_cls(ClList, Mod, ModS) :-
    findall(Cl, module_clause(clause, _, _, Mod, _, Cl), Cls),
    write_state_cls_(Cls, ClList, Mod, ModS).

write_state_cls_([], [], _, _).
write_state_cls_([Cl|Cls], [N|Ns], Mod, ModS) :-
    N = 1, !,
    write_cl(clause, Cl, ModS),
    write_state_cls_(Cls, Ns, Mod, ModS).
write_state_cls_([_|Cls], [_|Ns], Mod, ModS) :-
    write_state_cls_(Cls, Ns, Mod, ModS).

write_cl(module, Cl, S) :-
    portray_clause(S, Cl).
write_cl(directive, Cl, S) :-
    portray_clause(S, Cl).
write_cl(assertion, Cl, S) :-
    portray_clause(S, Cl).
write_cl(clause, Cl, S) :-
    portray_clause(S, Cl).

remove_modified_aux_files(DstDir) :-
    modified(Mod),
    remove_itf_file(DstDir, Mod),
    fail.
remove_modified_aux_files(_).

remove_itf_file(DstDir, Mod) :-
    path_concat(DstDir, Mod, BasePath),
    itf_filename(BasePath, Itf),
    del_file_nofail(Itf).

% ------------------------------------------------------------
% write the whole program
write_program(S, Mod) :-
    module_clause(A, _, _, Mod, _, Cl),
    write_cl(A, Cl, S),
    fail.
write_program(_, _).
