:- module(gather_stats, [main/1, gather_stats_dir/2], [assertions, datafacts]).

:- doc(title, "Gather the statistics of a modular analysis execution").

:- doc(author, "Isabel Garcia-Contreras").

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(format), [format/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [file_exists/1]).

:- use_module(ciaopp_tests(incanal/summarize_stat),
  [stat_directory_files_paths/2, write_total/2, summarize_file/1,
   check_stats_dir/2, all_tmp_stat/2]).

:- pred main(Args) : list(Args).
main([Dir]) :- !,
    ( check_stats_dir(Dir, FOut) -> true
    ; display('Not a statistics directory\n')
    ),
    open(FOut, write, SOut),
    gather_stats_dir(Dir, SOut).
main(_) :-
    display('Usage ./gather_stats_dir <stat_dir>\n'), nl.

gather_stats_dir(Dir, SOut) :-
    retractall_fact(all_tmp_stat(_, _)),
    
    ( % failure-driven loop
      stat_directory_file_path(Dir, F),
        ( file_exists(F) ->
            summarize_file(F),
            format(SOut, 'stats(', []),
            write_total(SOut, [it, load_time, comp_diff, restore, proc_diff, proc_assrts, preproc, inc_time, fixp, genreg, savereg]),
            retractall_fact(all_tmp_stat(_,_)),
            fail
        ;
            !
        )
    ;
        true
    ).

:- export(count/1).
count(1).
count(X1) :-
    count(X),
    X1 is X + 1.

% only works for incanal intermod stat files
:- export(stat_directory_file_path/2).
stat_directory_file_path(Dir, F) :-
    count(X),
    atom_number(Xa, X),
    atom_concat(Xa, '.stat', F1),
    path_concat(Dir, F1, F).
stat_directory_file_path(Dir, F) :-
    stat_directory_file_path(Dir, F).
