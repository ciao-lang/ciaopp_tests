:- module(y_prolog_read2, [read4/4], [assertions,nativeprops]).

:- use_module(syntax_error, [syntax_error/1, syntax_error/2]).

read4([Token|RestTokens],Precedence,Term,LeftOver) :-
    read(Token,RestTokens,Precedence,Term,LeftOver).
read4([],_X,_Y,_Z) :-
    syntax_error([expression,expected],[]).

:- trust success current_op(A,B,C) => (ground(A), ground(B), ground(C)).

:- data current_op/3.

expect(Token,[Token|Rest],Rest) :- !.
expect(Token,S0,_M) :-
    syntax_error([Token,or,operator,expected],S0).

prefixop(Op,Prec,Prec) :-
    current_op(Prec,fy,Op),
    !.
prefixop(Op,Prec,Less) :-
    current_op(Prec,fx,Op),
    !,
    Less is Prec-1.

postfixop(Op,Prec,Prec) :-
    current_op(Prec,yf,Op),
    !.
postfixop(Op,Less,Prec) :-
    current_op(Prec,xf,Op),
    !,
    Less is Prec-1.

infixop(Op,Less,Prec,Less) :-
    current_op(Prec,xfx,Op),
    !,
    Less is Prec-1.
infixop(Op,Less,Prec,Prec) :-
    current_op(Prec,xfy,Op),
    !,
    Less is Prec-1.
infixop(Op,Prec,Prec,Less) :-
    current_op(Prec,yfx,Op),
    !,
    Less is Prec-1.

ambigop(F,L1,O1,R1,L2,O2) :-
    postfixop(F,L2,O2),
    infixop(F,L1,O1,R1),
    !.

:- push_prolog_flag(multi_arity_warnings,off).

read(var(Variable,_M),['('|S1],Precedence,Answer,S) :-
    !,
    read4(S1,999,Arg1,S2),
    read_args(S2,RestArgs,S3),
    !,
    exprtl0(S3,apply(Variable,[Arg1|RestArgs]),Precedence,Answer,S).
read(var(Variable,_M),S0,Precedence,Answer,S) :-
    !,
    exprtl0(S0,Variable,Precedence,Answer,S).
read(atom(-),[integer(Integer)|S1],Precedence,Answer,S) :-
    Negative is-Integer,
    !,
    exprtl0(S1,Negative,Precedence,Answer,S).
read(atom(Functor),['('|S1],Precedence,Answer,S) :-
    !,
    read4(S1,999,Arg1,S2),
    read_args(S2,RestArgs,S3),
    Term=..[Functor,Arg1|RestArgs],
    !,
    exprtl0(S3,Term,Precedence,Answer,S).
read(atom(Functor),S0,Precedence,Answer,S) :-
    prefixop(Functor,Prec,Right),
    !,
    after_prefix_op(Functor,Prec,Right,S0,Precedence,Answer,S).
read(atom(Atom),S0,Precedence,Answer,S) :-
    !,
    exprtl0(S0,Atom,Precedence,Answer,S).
read(integer(Integer),S0,Precedence,Answer,S) :-
    !,
    exprtl0(S0,Integer,Precedence,Answer,S).
read('[',[']'|S1],Precedence,Answer,S) :-
    !,
    exprtl0(S1,[],Precedence,Answer,S).
read('[',S1,Precedence,Answer,S) :-
    !,
    read4(S1,999,Arg1,S2),
    read_list(S2,RestArgs,S3),
    !,
    exprtl0(S3,[Arg1|RestArgs],Precedence,Answer,S).
read('(',S1,Precedence,Answer,S) :-
    !,
    read4(S1,1200,Term,S2),
    expect(')',S2,S3),
    !,
    exprtl0(S3,Term,Precedence,Answer,S).
read(' (',S1,Precedence,Answer,S) :-
    !,
    read4(S1,1200,Term,S2),
    expect(')',S2,S3),
    !,
    exprtl0(S3,Term,Precedence,Answer,S).
read('{',['}'|S1],Precedence,Answer,S) :-
    !,
    exprtl0(S1,{},Precedence,Answer,S).
read('{',S1,Precedence,Answer,S) :-
    !,
    read4(S1,1200,Term,S2),
    expect('}',S2,S3),
    !,
    exprtl0(S3,{Term},Precedence,Answer,S).
read(string(List),S0,Precedence,Answer,S) :-
    !,
    exprtl0(S0,List,Precedence,Answer,S).
read(Token,S0,_X,_Y,_Z) :-
    syntax_error([Token,cannot,start,an,expression],S0).

:- pop_prolog_flag(multi_arity_warnings).

read_args([','|S1],[Term|Rest],S) :-
    !,
    read4(S1,999,Term,S2),
    !,
    read_args(S2,Rest,S).
read_args([')'|S],[],S) :- !.
read_args(S,_X,_Y) :-
    syntax_error([', or )',expected,in,arguments],S).

read_list([','|S1],[Term|Rest],S) :-
    !,
    read4(S1,999,Term,S2),
    !,
    read_list(S2,Rest,S).
read_list(['|'|S1],Rest,S) :-
    !,
    read4(S1,999,Rest,S2),
    !,
    expect(']',S2,S).
read_list([']'|S],[],S) :- !.
read_list(S,_X,_Y) :-
    syntax_error([', | or ]',expected,in,list],S).

after_prefix_op(Op,Oprec,_Aprec,S0,Precedence,_X,_Y) :-
    Precedence<Oprec,
    !,
    syntax_error([prefix,operator,Op,in,context,with,precedence,Precedence],S0).
after_prefix_op(Op,Oprec,_Aprec,S0,Precedence,Answer,S) :-
    peepop(S0,S1),
    prefix_is_atom(S1,Oprec),
    exprtl(S1,Oprec,Op,Precedence,Answer,S).
after_prefix_op(Op,Oprec,Aprec,S1,Precedence,Answer,S) :-
    read4(S1,Aprec,Arg,S2),
    Term=..[Op,Arg],
    !,
    exprtl(S2,Oprec,Term,Precedence,Answer,S).

peepop([atom(F),'('|S1],[atom(F),'('|S1]) :- !.
peepop([atom(F)|S1],[infixop(F,L,P,R)|S1]) :-
    infixop(F,L,P,R).
peepop([atom(F)|S1],[postfixop(F,L,P)|S1]) :-
    postfixop(F,L,P).
peepop(S0,S0).

prefix_is_atom([Token|_M],Precedence) :-
    prefix_is_atom(Token,Precedence).
prefix_is_atom(infixop(_X,L,_Y,_Z),P) :-
    L>=P.
prefix_is_atom(postfixop(_X,L,_Y),P) :-
    L>=P.
prefix_is_atom(')',_1).
prefix_is_atom(']',_1).
prefix_is_atom('}',_1).
prefix_is_atom('|',P) :-
    1100>=P.
prefix_is_atom(',',P) :-
    1000>=P.
prefix_is_atom([],_1).

exprtl0([atom(F)|S1],Term,Precedence,Answer,S) :-
    ambigop(F,L1,O1,R1,L2,O2),
    !,
    'exprtl0/5/1/$disj/1'(Term,Precedence,Answer,S,S1,F,L1,O1,R1,L2,O2).
exprtl0([atom(F)|S1],Term,Precedence,Answer,S) :-
    infixop(F,L1,O1,R1),
    !,
    exprtl([infixop(F,L1,O1,R1)|S1],0,Term,Precedence,Answer,S).
exprtl0([atom(F)|S1],Term,Precedence,Answer,S) :-
    postfixop(F,L2,O2),
    !,
    exprtl([postfixop(F,L2,O2)|S1],0,Term,Precedence,Answer,S).
exprtl0([','|S1],Term,Precedence,Answer,S) :-
    Precedence>=1000,
    !,
    read4(S1,1000,Next,S2),
    !,
    exprtl(S2,1000,(Term,Next),Precedence,Answer,S).
exprtl0(['|'|S1],Term,Precedence,Answer,S) :-
    Precedence>=1100,
    !,
    read4(S1,1100,Next,S2),
    !,
    exprtl(S2,1100,(Term;Next),Precedence,Answer,S).
exprtl0([atom(end_of_file)],'<eof>',_1,[],[]) :- !.
exprtl0([atom(end_of_file)],Term,_1,Term,[]) :- !.
exprtl0([Thing|S1],_X,_Y,_Z,_W) :-
    cant_follow_expr(Thing,Culprit),
    !,
    syntax_error([Culprit,follows,expression],[Thing|S1]).
exprtl0(S,Term,_1,Term,S).

'exprtl0/5/1/$disj/1'(Term,Precedence,Answer,S,S1,F,L1,O1,R1,_L2,_O2) :-
    exprtl([infixop(F,L1,O1,R1)|S1],0,Term,Precedence,Answer,S).
'exprtl0/5/1/$disj/1'(Term,Precedence,Answer,S,S1,F,_L1,_O1,_R1,L2,O2) :-
    exprtl([postfixop(F,L2,O2)|S1],0,Term,Precedence,Answer,S).

cant_follow_expr(atom(_1),atom).
cant_follow_expr(var(_1,_2),variable).
cant_follow_expr(integer(_1),integer).
cant_follow_expr(string(_1),string).
cant_follow_expr(' (',bracket).
cant_follow_expr('(',bracket).
cant_follow_expr('[',bracket).
cant_follow_expr('{',bracket).

exprtl([infixop(F,L,O,R)|S1],C,Term,Precedence,Answer,S) :-
    Precedence>=O,
    C=<L,
    !,
    read4(S1,R,Other,S2),
    Expr=..[F,Term,Other],
    exprtl(S2,O,Expr,Precedence,Answer,S).
exprtl([postfixop(F,L,O)|S1],C,Term,Precedence,Answer,S) :-
    Precedence>=O,
    C=<L,
    !,
    Expr=..[F,Term],
    peepop(S1,S2),
    exprtl(S2,O,Expr,Precedence,Answer,S).
exprtl([','|S1],C,Term,Precedence,Answer,S) :-
    Precedence>=1000,
    C<1000,
    !,
    read4(S1,1000,Next,S2),
    exprtl(S2,1000,(Term,Next),Precedence,Answer,S).
exprtl(['|'|S1],C,Term,Precedence,Answer,S) :-
    Precedence>=1100,
    C<1100,
    !,
    read4(S1,1100,Next,S2),
    exprtl(S2,1100,(Term;Next),Precedence,Answer,S).
exprtl(S,_1,Term,_2,Term,S).



