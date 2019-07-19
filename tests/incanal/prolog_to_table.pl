:- module(prolog_to_table, [main/1], [assertions,hiord,datafacts]).

% TODO: doc
% Add parameter for preprocessing the data, e.g., normalization

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(format), [format/3]).
:- use_module(library(read), [read/2]).

% Opts should be a predicate for preprocessing the tuples, e.g., normalization
main([FileIn, FileOut, OutFormat|Opts]) :- !,
	set_opt(OutFormat),
	read_file(FileIn),
	process_tuples(Opts),
	write_file(FileOut).
main(_) :-
	display('Wrong parameters\n').

:- data opt/2.
opt(output_format, latex). % default output format

set_opt(X) :- % output options
	(X = latex ; X = csv ; X = gnuplot), !,
	retractall_fact(opt(output_format, _)),
	assertz_fact(opt(output_format, X)).

read_file(FileIn) :-
	retractall_fact(tuple(_)),
	open(FileIn, read, S),
	repeat,
	read(S,X),
	(X = end_of_file, !
	    ; assertz_fact(tuple(X)),
	      fail
	),
	close(S).

% assuming all the statistics are of the same type

:- data tuple/1.

% enumerates the data of the table (to print by bactracking later)
table_data(Tuple) :-
	tuple(T),
	T =.. [_|Tuple].

write_file(FileOut) :-
	retractall_fact(first_total(_)),
	opt(output_format, OutF),
	open(FileOut, write, OutS),
	write_table_header(OutF, OutS),
	write_table_data(OutF, OutS),
	write_table_footer(OutF, OutS),
	close(OutS).

data_format(latex, ' & ', ' \\\\ ~n').
data_format(csv, ' ; ', '~n').
data_format(gnuplot, ' \t ', '~n').

data_type_format(0't, '~q').
data_type_format(0'0, '~3f').
data_type_format(0'1, '~3f').

write_table_header(_, _) :- 
 	retract_fact(tuple(_)). % Do not use header for any format
% 	T =.. [_|ColTitle],
% 	%format(OutS, '\#', []), % necessary?
% 	data_type_format(t, F)
% 	print_tuple(ColTitle, OutS, '\t', '\n').

print_tuple([D], S, _, EndF) :- !,
	print_data_and_splitter(S, D, EndF).
print_tuple([D|Ds], S, SplitF, EndF) :- % first data
	print_data_and_splitter(S, D, SplitF),
	print_tuple(Ds, S, SplitF, EndF).

print_data_and_splitter(S, D, EndF) :-
	next_proc(X),
	data_type_format(X, DF),
	format(S, DF, [D]),
	format(S, EndF, []).

write_table_data(OutF, OutS) :-
	data_format(OutF, SplitFormat, EndFormat),
	( % failure-driven loop
	  table_data(T),
	    proc_data(T, NT),
	    print_tuple(NT, OutS, SplitFormat, EndFormat),
	    fail
	; true).

write_table_footer(latex, _) :- !.
write_table_footer(_, _).

% TODO: option to not print 0s

proc_data(D, [Acc]) :-
	opt(proc, sum), !,
	acc_tuple_values(D, 0, Acc).
proc_data(D, ND) :-
	opt(proc, norm), !,
	normalize_with_first(D, ND).
proc_data(D, D).

:- data proc/1.
assert_proc([]).
assert_proc([C|Cs]) :-
	assertz_fact(proc(C)),
	assert_proc(Cs).

% get data and store to keep order
next_proc(C) :-
	retract_fact(proc(X)), !,
	assertz_fact(proc(X)),
	C = X.
next_proc(0'1).

process_tuples([]).
process_tuples([ProcessConfig]) :- !,
	set_process_config(ProcessConfig).
process_tuples([sum, ProcessConfig]) :- !,
% add all the values that conform the analysis
	assertz_fact(opt(proc, sum)),
	set_process_config(ProcessConfig).
process_tuples([norm, ProcessConfig]) :-
	assertz_fact(opt(proc, norm)),
	set_process_config(ProcessConfig).

set_process_config(PC) :-
	retractall_fact(proc(_)),
	atom_codes(PC, Cs),
	assert_proc(Cs).

% predicates for processing the statistics
:- pred normalize_with_first(Tuple, NTuple)
	#"Normalizes the data making the first row to be 1.".
:- data first_total/1.

normalize_with_first(Data, NDs) :-
	first_total(Total), !,
	normalize_with_first_(Data, Total, NDs).
normalize_with_first(Data, NDs) :-
	acc_tuple_values(Data, 0, Total),
	set_fact(first_total(Total)),
	normalize_with_first_(Data, Total, NDs).

acc_tuple_values([], Acc, Acc).
acc_tuple_values([D|Ds], Acc, NAcc) :-
	next_proc(X),
	use_value(X, D, V),
	Acc1 is Acc + V,
	acc_tuple_values(Ds, Acc1, NAcc).

use_value(0't, _, 0).
use_value(0'0, _, 0).
use_value(0'1, V, V).

normalize_with_first_([], _, []).
normalize_with_first_([D|Ds], Total, [ND|NewDs]) :-
	next_proc(X),
	( X = 0't ->  % title
	    ND = D
	; 
	    use_value(X, D, V),
	    ND is V / Total
	),
	normalize_with_first_(Ds, Total, NewDs).
