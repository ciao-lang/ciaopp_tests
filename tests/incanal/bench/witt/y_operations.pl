:- module(_, [dij/2, wc/2, cc/3, distance/3, combine/3], []).

:- use_module(library(lists), [length/2]).

:- use_module(my_library, [my_own_ordintersection/3, my_own_append/3, my_own_ordunion/3]).

dij(M, Dij):-
    dij(M, 0, Num, 0, D),
    nat_log(D, L),
    Den is L*D,
    divide(Num, Den, Dij).

divide(X, X, 1).
divide(X, Y, R):-
    X =\= Y,
    R is X / Y.

dij([], X, X, Y, Y):- !.          %% Green
dij([M|Ms], Xi, Xo, Yi, Yo):- !,  %% Green
    dij(M, Xi, Xm, Yi, Ym),
    dij(Ms, Xm, Xo, Ym, Yo).
dij(A, Xi, Xo, Yi, Yo):-
    number(A), !,              %% Green
    Yo is Yi + A,
    nat_log(A, L),
    Xo is Xi + A * L.

wc(ml(_, _, Z, Conts), WC):-
    wc(Conts, 0, Out),
    WC is Out / Z.
wc([], A, A).
wc([mat(D, _, _, _)|Ms], I, O):-
    I1 is I + D,
    wc(Ms, I1, O).

distance(ml(_, Ats1, _, _), ml(_, Ats2, _, _), D):-
    distance(Ats1, Ats2, 1, D1),
    D is 1 / D1.
distance([], [], D, D).
distance([A-V1s|A1s], [A-V2s|A2s], In, Out):-
    my_own_ordintersection(V1s, V2s, Vs),
    length(Vs, L),
    Mid is In + L,
    distance(A1s, A2s, Mid, Out).

bck(C1, C2, B):-
    combine(C1, C2, C12),
    wc(C1, W1),
    wc(C2, W2),
    wc(C12, W12),
    B is 1 / (W1 + W2 - 2*W12).

oc([], _, N, In, Out):-
    Out is In / N.
oc([C|Cs], Cat, N, In, Out):-
    N1 is N + 1,
    bck(C, Cat, Bck),
    Mid is In + Bck,
    oc(Cs, Cat, N1, Mid, Out).

cc(Cat, Cats, C):-
    wc(Cat, Wc),
    oc(Cats, Cat, 0, 0, Oc),
    C is Wc / Oc.

combine(ml(Nm1, Cars1, Z, M1), ml(Nm2, Cars2, Z, M2), ml(Nm3, Cars3, Z, M3)):-
    my_own_append(Nm1, Nm2, Nm3),
    combine_cars(Cars1, Cars2, Cars3),
    combine_conts(M1, M2, M3).

combine_cars([], [], []).
combine_cars([A-V1s|C1s], [A-V2s|C2s], [A-V3s|C3s]):-
    my_own_ordunion(V1s, V2s, V3s),
    combine_cars(C1s, C2s, C3s).

combine_conts([], [], []).
combine_conts([mat(_, Ai, Aj, M1)|M1s], [mat(_, Ai, Aj, M2)|M2s], [mat(D, Ai,
    Aj,M3)|M3s]):-
    add_matrices(M1, M2, M3),
    dij(M3, D),
    combine_conts(M1s, M2s, M3s).

add_matrices([], [], []).
add_matrices([M1|M1s], [M2|M2s], [M3|M3s]):-
    add_matrices(M1, M2, M3),
    add_matrices(M1s, M2s, M3s).
add_matrices(E1, E2, E3):-
    number(E1),
    E3 is E1 + E2.

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% This definition can be changed to work in particular systems.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nat_log(X,-12):- X =< 0.00001, !.       % This may happen in the program
nat_log(X,Y):- mylog(X, Y).             % Adapt mylog for your system.
 
mylog(Number, Log):-
    N is Number - 1,
    N2 is N * N,
    N3 is N2 * N,
    N4 is N3 * N,
    N5 is N4 * N,
    Log is N - N2/2 + N3/3 - N4/4 + N5/5.

