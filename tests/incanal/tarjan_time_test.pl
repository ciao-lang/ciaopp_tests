:- module(_, [main/1], []).

:- use_module(ciaopp(frontend_driver), [module/1]).
:- use_module(ciaopp(raw_printer), [raw_output/1]).

:- use_module(ciaopp(p_unit), [program/2]).

:- use_module(ciaopp(plai/transform), [transform_clauses/5]).
:- use_module(ciaopp(plai/tarjan), [tarjan/2, recursive_classify/4]).
:- use_module(ciaopp(plai/program_tarjan), [program_tarjan/1, program_recursive_classify/3]).

:- use_module(ciaopp(analysis_stats), [stat_no_store/2]).

:- use_module(engine(io_basic)).

main([File, Tarjan]) :- !,
	module(File), !,
	program(Cls, Ds),
	generate_trans_clauses(Tarjan, Cls, Ds).

generate_trans_clauses(Tarjan, Cls, Ds) :-
	classify_clauses(Tarjan, Cls, Rs, Ps),
	transform_clauses(Cls,Ds,Rs,Ps,gr),
	raw_output(user).

classify_clauses(assert_tarjan, Cls, Rs, Ps) :-
	stat_no_store(program_tarjan(Cls), S1),
	display(S1), nl,
	stat_no_store(program_recursive_classify(Cls,Rs,Ps), S2),
	display(S2), nl.
classify_clauses(list_tarjan, Cls, Rs, Ps) :-
	stat_no_store(tarjan(Cls, SCCs), S1),
	display(S1), nl,
	stat_no_store(recursive_classify(Cls,SCCs,Rs,Ps), S2),
	display(S2), nl.