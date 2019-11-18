:- module(syntax_error,[ syntax_error/1, syntax_error/2 ], [datafacts]).

:- use_module(engine(io_basic)).
:- use_module(library(lists), [length/2]).
:- use_module(library(old_database), [recorda/3, recorded/3]).
:- use_module(library(ttyout), [ttynl/0, ttyput/1]).

syntax_error(Message,List) :-
    ttynl,
    display(**),
    my_display_list(Message),
    length(List,Length),
    recorda(syntax_error,length(Length),_M),
    !,
    fail.

my_display_list([Head|Tail]) :-
    ttyput(32),
    display_token(Head),
    !,
    my_display_list(Tail).
my_display_list([]) :-
    ttynl.

:- push_prolog_flag(multi_arity_warnings,off).

syntax_error(List) :-
    recorded(syntax_error,length(AfterError),Ref),
    erase(Ref),
    length(List,Length),
    BeforeError is Length-AfterError,
    display_list(List,BeforeError),
    !.

:- pop_prolog_flag(multi_arity_warnings).

display_list(X,0) :-
    nl,
    display('<<here>> '),
    !,
    nl,
    display_list(X,99999).
display_list([Head|Tail],BeforeError) :-
    display_token(Head),
    ttyput(32),
    Left is BeforeError-1,
    !,
    display_list(Tail,Left).
display_list([],_N) :-
    ttynl.

display_token(atom(X)) :-
    !,
    display(X).
display_token(var(_V,X)) :-
    !,
    display(X).
display_token(integer(X)) :-
    !,
    display(X).
display_token(string(X)) :-
    !,
    display(X).
display_token(X) :-
    display(X).




