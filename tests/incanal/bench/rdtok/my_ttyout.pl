:- module(my_ttyout, [ttynl/0, ttyput/1], []).

:- use_module(engine(io_basic)).

ttyput(X) :- put_code(user, X).

ttynl :- nl(user).
