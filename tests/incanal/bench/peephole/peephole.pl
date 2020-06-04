:- module(peephole, [peephole_opt/2], [assertions]).

:- use_module(peephole1).
:- use_module(peephole2).
:- use_module(peephole3).
:- use_module(peephole4).

:- entry peephole_opt(X,Y) : ( var(Y), ground(X) ).

peephole_opt(Pil,OptPil) :-
    popt1(Pil,Pil1),
    popt2(Pil1,Pil2),
    popt3(Pil2,Pil3),
    popt4(Pil3,_N,OptPil).
