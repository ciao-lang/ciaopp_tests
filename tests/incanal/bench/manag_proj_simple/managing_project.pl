:- module(managing_project,
    [available_hours/6,
     effort/6,
     personal_effort/7,
     project_effort/7,
     personal_project_effort/8,
     monthly_plan/4,
     cost_statement/6,
     cost_statement_personnel/6,
     global_cost_statement/5,
     check_plan/0,
     check_all_availabilities/4,
     person_load_overview/4,
     time_sheet/4,
     hours_to_be_justified_person_project/7,
     hours_to_be_justified_person/6,
     hours_to_be_justified_project/6
    ],
     [assertions]).

:- use_module(engine(io_basic)).
:- use_module(library(format)).
:- use_module(library(lists), [member/2, append/3]).

:- use_module(committed_hours).
:- use_module(planned).
:- use_module(dates_and_facts).
:- use_module(responsible_persons_upm).
:- use_module(payments).

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


:- entry effort(A,B,C,D,E,F) : ground([A,B,C,D,E]).

effort(SM,SY,EM,EY,Unit,Effort):-
    get_list_projects(L),
    all_project_efforts(L,SM,SY,EM,EY,Unit,Effort).
    
all_project_efforts([],_SM,_SY,_EM,_EY,_Unit,0).
all_project_efforts([Project|Projects],SM,SY,EM,EY,Unit,MMs):-
    project_effort(Project,SM,SY,EM,EY,Unit,Effort),
    all_project_efforts(Projects,SM,SY,EM,EY,Unit,MMs0),
    MMs is MMs0 + Effort.

:- doc(personal_effort(+Person,+SM,+SY,+EM,+EY,+Unit,-Effort),
     "Computes in @var{Effort} the number of units which @var{Person}
     has to devote to all projects in the corresponding time
     period.").

:- entry personal_effort(A,B,C,D,E,F,G) : ground([A,B,C,D,E,F]).

personal_effort(Person,SM,SY,EM,EY,Unit,Effort):-
    get_list_projects(L),
    personal_all_project_effort(L,Person,SM,SY,EM,EY,Unit,Effort).
    
personal_all_project_effort([],_Person,_SM,_SY,_SM1,_SY1,_Unit,0).
personal_all_project_effort([P|Projects],Person,SM,SY,SM1,SY1,Unit,TP):-
    personal_project_effort(Person,P,SM,SY,SM1,SY1,Unit,Effort),
    personal_all_project_effort(Projects,Person,SM,SY,SM1,SY1,Unit,TP0),
    TP is TP0 + Effort.
    

:- doc(project_effort(+Project,+SM,+SY,+EM,+EY,+Unit,-Effort),
     "Returns in @var{Effort} the time in @var{Unit}s promised for
     @var{Project} in the period indicated.").

:- entry project_effort(A,B,C,D,E,F,G): ground([A,B,C,D,E,F]).

project_effort(Project,SM,SY,EM,EY,Unit,Effort):-
    convert_date(Project,SM,SY,Month),
    convert_date(Project,EM,EY,Month1),
    planned(Project,Month,Month1,1,Unit,_Tasks,_Efforts,Effort).

:- doc(personal_project_effort(+Person,+Project,+SM,+SY,+EM,+EY,+Unit,-Effort),
     "Returns in @var{Effort} the time in @var{Unit}s promised by
     @var{Person} for @var{Project} in the period indicated.").

:- entry personal_project_effort(A,B,C,D,E,F,G,H): ground([A,B,C,D,E,F,G]).

personal_project_effort(Person,Project,SM,SY,EM,EY,Unit,Effort):-
    convert_date(Project,SM,SY,Month),
    convert_date(Project,EM,EY,Month1),
    planned(Project,Month,Month1,1,Unit,Tasks,Efforts,_),
    add_up_hours(Tasks,Efforts,Project,1,[],Assignments),
    member(person(Person,Effort,_,_),Assignments),!.
personal_project_effort(_Person,_P,_SM,_SY,_SM1,_SY1,_Unit,0).


:- doc(monthly_plan(+SM,+SY,+EM,+EY), "Prints out the work
      expected in each of the months in the period indicated.").

monthly_plan(SM,SY,EM,EY):-     
    get_list_projects(Projects),
    show_project_monthly_plan(SM,SY,EM,EY,Projects).
show_project_monthly_plan(M,Y,M,Y,_Projects):-!.
show_project_monthly_plan(SM,SY,EM,EY,Projects):-       
    next_month(SM,SY,SM1,SY1),
    all_project_efforts(Projects,SM,SY,SM1,SY1,mm,MMs),
    format("~a ~d Total ~2f: ",[SM,SY,MMs]),
    show_project_effort(Projects,SM,SY,SM1,SY1),
    show_project_monthly_plan(SM1,SY1,EM,EY,Projects).

show_project_effort([],_SM,_SY,_EM,_EY):-
    nl.
show_project_effort([P|Projects],SM,SY,EM,EY):-
    project_effort(P,SM,SY,EM,EY,mm,Effort),
    format("(~a, ~2f)",[P,Effort]),
    show_project_effort(Projects,SM,SY,EM,EY).


:- doc(person_load_overview(+SM,+SY,+EM,+EY), "Prints out the load
   of all members of the group during the period mentioned.").

person_load_overview(SM,SY,EM,EY):-
    get_list_people(People),
    show_person_load(People,SM,SY,EM,EY).

show_person_load([],_SM,_SY,_EM,_EY):-
    nl.
show_person_load([P|People],SM,SY,EM,EY):-
    personal_effort(P,SM,SY,EM,EY,mm,Effort),
    format("(~a, ~2f)~n",[P,Effort]),
    show_person_load(People,SM,SY,EM,EY).


:- doc(available_hours(+Person,+SM,+SY,+EM,+EY,-Hours), "Computes
   in @var{Hours} the number of hours which remain available for
   @var{Person} in the period indicated.").

:- entry available_hours(A,B,C,D,E,F): ground([A,B,C,D,E]).

available_hours(_Person,M,Y,M,Y,0):-!.
available_hours(Person,SM,SY,EM,EY,Hours):-
    committed_hours(SM,SY,Person,Teaching,Other,Holiday),!,
    max_hours_per_month(Max),
    Available0 is Max - (Teaching +Other +Holiday),
    (Available0 < 0 -> Available = 0
    ;
        Available = Available0),
    next_month(SM,SY,SM1,SY1),
    personal_effort(Person,SM,SY,SM1,SY1,hour,TP),
    Still_Available is Available -TP,
    available_hours(Person,SM1,SY1,EM,EY,Tmp_Hours),
    Hours is Tmp_Hours + Still_Available.
available_hours(Person,SM,SY,EM,EY,Hours):-
    next_month(SM,SY,SM1,SY1),
    available_hours(Person,SM1,SY1,EM,EY,Hours).

%%%%%%%%%%%%%%%%%
add_up_hours([],[],_,_,Assignments,Assignments).
add_up_hours([Task|Tasks],[E|Es],Project,Partner,Tmp,Assignments):-
    Task = t(WP,T,_S,_E,_N),
    responsible(Project,WP,T,Partner,Workers,Ratios),
    insert(Workers,Ratios,E,Task,Tmp,N_Tmp), 
    add_up_hours(Tasks,Es,Project,Partner,N_Tmp,Assignments).


insert([],[],_E,_T,Tmp,Tmp).
insert([W|Workers],[R|Ratios],E,T,Tmp,Final):-
    Num_Hours  is E * R,
    add_worker(Tmp,W,Num_Hours,T,N_Tmp),
    insert(Workers,Ratios,E,T,N_Tmp,Final).


add_worker([],W,Num_Hours,T,N_Tmp):-
    N_Tmp = [person(W,Num_Hours,[T],[Num_Hours])].
add_worker(Tmp,W,Num_Hours,T,N_Tmp):-
    Tmp = [person(W,Hours,Tasks,Efforts)|Persons], !,
    N_Hours is Hours + Num_Hours,
    append(Tasks,[T],N_Tasks),
    append(Efforts,[Num_Hours],N_Efforts),
    N_Tmp = [person(W,N_Hours,N_Tasks,N_Efforts)|Persons].
add_worker([P|Persons],W,Num_Hours,T,[P|N_Tmp]):-
    add_worker(Persons,W,Num_Hours,T,N_Tmp).

:- doc(cost_statement(+Project,+SM,+SY,+EM,+EY,-Efforts),
   "Computes the number of hours which each participant has devoted
   (should devote) to @var{Project} in the corresponding period.").

cost_statement(Project,SM,SY,EM,EY,Efforts):-
    my_findall(P,person(P),Persons),
    get_all_personal_project_efforts(Persons,Project,SM,SY,EM,EY,Efforts).

:- doc(cost_statement_personnel(+Project,+SM,+SY,+EM,+EY,-Euros),
   "Computes the cost in euros which correspond to the given period
   for the work performed in @var{Project}.").

cost_statement_personnel(Project,SM,SY,EM,EY,Euros):-
    cost_statement(Project,SM,SY,EM,EY,Efforts),
    add_all_costs(Efforts,Euros).

add_all_costs([],0).
add_all_costs([(Person,Hours)|Efforts],Euros):-
    add_all_costs(Efforts,TmpEuros),
    category(Person,Cat),
    cost_per_hour(Cat,Cost_Hour),
    Euros is TmpEuros + Hours * Cost_Hour.

:- doc(global_cost_statement(+SM,+SY,+EM,+EY,-Efforts), "Returns
     in @var{Efforts} the hours devoted by each member of the group to
     European projects within the specified interval.").


global_cost_statement(SM,SY,EM,EY,Efforts):-
    my_findall(P,person(P),Persons),
    get_all_efforts(Persons,SM,SY,EM,EY,Efforts).

get_all_efforts([],_SM,_SY,_EM,_EY,[]).
get_all_efforts([Person|Persons],SM,SY,EM,EY,Efforts):-
    personal_effort(Person,SM,SY,EM,EY,hours,Eff),
    (Eff = 0 ->
        Efforts = More_Efforts
    ;
        Efforts = [(Person,Eff)|More_Efforts]),
    get_all_efforts(Persons,SM,SY,EM,EY,More_Efforts).

:- doc(time_sheet(+Person,+Month,+Year,-Efforts), "Computes a
     plausible time-table for @var{Person} during @var{Month} of
     @var{Year} by returning in @var{Efforts} the hours devoted to
     different issues and projects.").

time_sheet(Person,SM,SY,Efforts):-
    committed_hours(SM,SY,Person,Teaching,Other,Holiday),!,
    Efforts = [(avail,Still_Available),(teach,Teaching),(other,Other),(vacat,Holiday)|MoreEfforts],
    max_hours_per_month(Max),
    Available0 is Max - (Teaching +Other +Holiday),
    (Available0 < 0 -> Available = 0
    ;
        Available = Available0),
    next_month(SM,SY,SM1,SY1),
    get_list_projects(Projects),
    get_all_personal_efforts(Projects,Person,SM,SY,SM1,SY1,MoreEfforts),
    personal_effort(Person,SM,SY,SM1,SY1,hour,TP),
    Still_Available is Available -TP.
    
get_all_personal_efforts([],_Person,_SM,_SY,_EM,_EY,[]).
get_all_personal_efforts([Project|Projects],Person,SM,SY,EM,EY,Efforts):-
    personal_project_effort(Person,Project,SM,SY,EM,EY,hours,Eff),
    (Eff = 0 ->
        Efforts = More_Efforts
    ;
        Efforts = [(Project,Eff)|More_Efforts]),
    get_all_personal_efforts(Projects,Person,SM,SY,EM,EY,More_Efforts).



:- doc(check_plan/0, "Checks that in the whole planning period
   there is nobody with negative availability.").

check_plan:-
    start_period(SM,SY),
    end_period(EM,EY),
    check_all_availabilities(SM,SY,EM,EY).


:- doc(check_all_availabilities(+SM,+SY,+EM,+EY), "Checks that in
     the considered period there is nobody with negative
     availability.").

check_all_availabilities(SM,SY,EM,EY):-
    my_findall(P,person(P),Persons),
    check_each_availability(Persons,SM,SY,EM,EY).

    
check_each_availability([],_SM,_SY,_EM,_EY).
check_each_availability([Person|Persons],SM,SY,EM,EY):-
    check_availability(Person,SM,SY,EM,EY),
    check_each_availability(Persons,SM,SY,EM,EY).

check_availability(_Person,M,Y,M,Y):-!.
check_availability(Person,SM,SY,EM,EY):-
    next_month(SM,SY,SM1,SY1),
    available_hours(Person,SM,SY,SM1,SY1,Hours),
    (Hours >= 0 ->
      true
    ;
        format("In ~a ~d ~a has negative availability: ~2f~n", [SM,SY,Person,Hours])),
    check_availability(Person,SM1,SY1,EM,EY).

get_all_personal_project_efforts([],_Project,_SM,_SY,_EM,_EY,[]).
get_all_personal_project_efforts([Person|Persons],Project,SM,SY,EM,EY,Efforts):-
    personal_project_effort(Person,Project,SM,SY,EM,EY,hours,Eff),
    (Eff = 0 ->
        Efforts = More_Efforts
    ;
        Efforts = [(Person,Eff)|More_Efforts]),
    get_all_personal_project_efforts(Persons,Project,SM,SY,EM,EY,More_Efforts).

:- doc(hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours),
   "The aim of this predicate is to compute the number of hours which
   @var{Person} should justify in the Cost Sstatement of @var{Project}
   for the period indicated in order to match the actual payments.").

:- entry hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours):
    ground([Person,Project,SM,SY,EM,EY]).

hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours):-
    received(Person,Project,SM,SY,EM,EY,Amount),
    category(Person,Cat),
    cost_per_hour(Cat,Cost_Hour),
    Hours is Amount/Cost_Hour.

:- doc(hours_to_be_justified_person(Person,SM,SY,EM,EY,Hours),
   "The aim of this predicate is to compute the number of hours which
   @var{Person} should justify in the Cost Statement of each running
   project for the period indicated in order to match the actual
   payments.").

hours_to_be_justified_person(Person,SM,SY,EM,EY,Hours):-
    get_list_projects(Projects),
    all_hours_to_justify(Projects,Person,SM,SY,EM,EY,Hours).

all_hours_to_justify([],_Person,_SM,_SY,_EM,_EY,[]).
all_hours_to_justify([Project|Projects],Person,SM,SY,EM,EY,[(Project,Hours)|More]):-
    hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours),
    all_hours_to_justify(Projects,Person,SM,SY,EM,EY,More).

:- doc(hours_to_be_justified_project(Project,SM,SY,EM,EY,Hours),
   "The aim of this predicate is to compute the number of hours which
   should be justified in the Cost Statement of @var{Project} for each
   person in period indicated in order to match the actual
   payments.").

hours_to_be_justified_project(Project,SM,SY,EM,EY,Hours):-
    get_list_people(People),
    all_hours_to_justify_project(People,Project,SM,SY,EM,EY,Hours).
    
all_hours_to_justify_project([],_Project,_SM,_SY,_EM,_EY,[]).
all_hours_to_justify_project([noone|People],Project,SM,SY,EM,EY,More):-
    !,
    all_hours_to_justify_project(People,Project,SM,SY,EM,EY,More).
all_hours_to_justify_project([Person|People],Project,SM,SY,EM,EY,Result):-
    hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours),
    (Hours =:= 0 ->
        Result = More
    ;
        Result = [(Person,Hours)|More]),
    all_hours_to_justify_project(People,Project,SM,SY,EM,EY,More).

