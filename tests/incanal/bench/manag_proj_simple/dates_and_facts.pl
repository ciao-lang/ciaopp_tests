:- module(dates_and_facts,
    [person/1,
     get_list_people/1,
     next_month/4,
     check_date_and_convert/4,
     convert_date/4,
     convert_date_origin/3,
     month_to_number/2,
     start_period/2,
     end_period/2,
     project/1,
     get_list_projects/1,
     max_hours_per_month/1,
     hours_per_month/1,
     max_hours_per_year/1,
     cost_per_hour/2,
     category/2,
     date_less_than/4,
     date_less_or_equal/4
    ],
    [assertions]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_package(datafacts).
:- data d/1.
my_findall(Tmpl, G, Xs) :-
    asserta_fact(d(first)),
    my_collect(Tmpl, G),
    my_sols([],Xs0), Xs=Xs0.
my_collect(Tmpl, G) :-
    ( my_call(G),
      asserta_fact(d(sol(Tmpl))),
      fail
    ; true
    ).
my_sols(Xs0,Xs) :-
    retract_fact(d(Mark)),
    !,
    ( Mark = first -> Xs = Xs0
    ; Mark = sol(X) -> Xs1 = [X|Xs0], my_sols(Xs1, Xs)
    ).
my_call(person(A)) :- person(A).

% :- trust pred person(A) => ground(A).
% :- trust pred get_list_people(A) => ground(A).
% :- trust pred max_hours_per_month(A) => ground(A).
% :- trust pred hours_per_month(A) => ground(A).
% :- trust pred max_hours_per_year(A) => ground(A).
% :- trust pred next_month(A,B,C,D) => ground([A,C]), mshare([[B,D]]).
% :- trust pred month_to_number(A,B) => ground([A,B]).
% :- trust pred project(A) => ground(A).
% :- trust pred get_list_projects(A) => ground(A).
% :- trust pred start_period(A,B) => ground([A,B]).
% :- trust pred end_period(A,B) => ground([A,B]).
% :- trust pred cost_per_hour(A,B) => ground([A,B]).
% :- trust pred category(A,B) => ground([A,B]).

person(german).
person(noone).

get_list_people(L):-
    my_findall(P, person(P), L).

max_hours_per_month(143).

hours_per_month(131.25).

max_hours_per_year(1575).

next_month(jan,Y,feb,Y).
next_month(dec,Y,jan,Y1):-
    Y1 is Y + 1.

check_date_and_convert(asap,M,Y,Month):-!,
    convert_date_origin(M,Y,Date),
    Date >= 10,
    Date < 46, 
    Month is Date -10.
check_date_and_convert(colognet,M,Y,Month):-!,
    convert_date_origin(M,Y,Date),
    Date >= 0,
    Date < 36, 
    Month is Date.
check_date_and_convert(amos,M,Y,Month):-
    convert_date_origin(M,Y,Date),
    Date >= 2,
    Date < 26, 
    Month is Date -2.

convert_date(asap,M,Y,Month):-
    convert_date_origin(M,Y,Date),
    Month is Date -10.
convert_date(colognet,M,Y,Month):-
    convert_date_origin(M,Y,Month).
convert_date(amos,M,Y,Month):-
    convert_date_origin(M,Y,Date),
    Month is Date -2.

convert_date_origin(M,Y,Date):-
    month_to_number(M,M_Num),
    Date is M_Num -1 + 12*(Y-2).

month_to_number(jan,1).

project(asap).
project(amos).
project(colognet).

get_list_projects(L):-
    my_findall(P, project(P), L).

start_period(jan,02).

end_period(nov,05).

cost_per_hour(prof,39.49).

category(german,asste_prof).
category(noone,_):- fail.

date_less_than(_M1,Y1,_M2,Y2):-
    Y1 < Y2,!.
date_less_than(M1,Year,M2,Year):-
    M1 < M2.

date_less_or_equal(M,Y,M,Y):-!.
date_less_or_equal(M1,Y1,M2,Y2):-
    date_less_than(M1,Y1,M2,Y2).
