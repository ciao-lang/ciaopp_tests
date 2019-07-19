:- module(_, [], [assertions]).

:- doc(filetype,application).

:- doc(title,"CiaoPP modular program benchmark driver").

:- doc(author, "Isabel Garcia-Contreras").

:- doc(summary, "incanal_intermod_bench_driver is a command that
allows the user to simulate the edition-analysis iterative process by
simulating a edition sequence of a given (full) program.").

:- doc(module,
"
Run the benchmark of modular analysis with incremental modular analysis.

@section{How to use it:}

@begin{enumerate}
@item Compile the command @tt{ciaoc incanal_intermod_bench_driver}.
@item Execute @tt{./incanal_intermod_bench_driver <bench_dir_name> <add/del> <#changes/iteration> <domain> Opts}.
@item Example (with no options):

@begin{verbatim}
./incanal_intermod_bench_driver qsort add 1 gr
@end{verbatim}

will perform a test of adding clauses 1 by 1 in the files of benchmark
qsort with a monolithic analysis of groundness.

Options can refer to:

@begin{itemize}
@item Configurations of CiaoPP.
@item Benchmark edition simulation options.
@end{itemize}

@end{enumerate}

@section{Supported configurations of CiaoPP}

The following configurations are working, monospace words are the ones
used to specify the option to the program in @bf{any order}. If no options
are specified, the default configuration is selected.

@begin{itemize} 
@item Fixpoints
@begin{itemize}
@item @tt{plai} (non incremental)
@item @tt{dd}  (default)
@item @tt{di}  (non incremental)
@end{itemize}

@item Modularity
@begin{itemize} 
@item @tt{monolithic}
@item @tt{modular} (default)
@end{itemize}

@item Incremental (@tt{incremental})
@begin{itemize} 
@item Adding
@item Deleting (Options: @tt{top_down} (default), @tt{bottom_up})
@end{itemize}

@end{itemize}

@section{Edition simulation options}

@subsection{Grouping (@tt{--group <Opt>})} @bf{Not implemented yet!}
@begin{itemize}
@item @tt{clause} (default): Clause (default), add in groups of clauses of size #changes/iteration.
@item @tt{pred}: Predicate, add in groups of predicates of size #changes/iteration.
@end{itemize}

@subsection{Editing module by module (@tt{--one_mod <Opt>})}

This option controls whether to add/delete clauses in a module until it is
complete/empty before switching to another module.

@begin{itemize}
@item @tt{yes}: do not edit other modules until a module is finished (empty or complete).
@item @tt{no}: perform editions in all modules.
@end{itemize}

@subsection{Order of sequence}
@begin{itemize}
@item Ordered (default) (following the rules of the sequence configuration).
@item Random (@tt{--rand <Seed>}) option and first argument as the seed.
@end{itemize}

@subsection{Limiting the sequence}
Sequences can be limited in the beginning (do not start from an empty/full
program) and to a number of steps (i.e., performing only a number of changes).

This is controled with option @tt{--start N} and @tt{--steps M}, by
default, these are @bf{absolute} (@tt{--seq_sz abs}) numbers, (different for
each of the benchmarks). To express @tt{N} and @tt{M} in
\% of the size of the program the option sequence sizes relative
@tt{--seq_sz rel} has to be specified.

For example:

@begin{verbatim}
./incanal_intermod_bench_driver hanoi add 2 shfr --rand 2132 --start 20 --steps 80 --seq_sz rel
@end{verbatim}

Will simulate editing the program hanoi starting from a random program
containing 20\% of the clauses and adding clauses (randomly) 2 by 2
until the program contains 80\% of the original program clauses.
	").

% TODO: add more examples of execution
