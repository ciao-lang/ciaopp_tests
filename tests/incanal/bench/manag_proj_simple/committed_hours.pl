:- module(committed_hours,[committed_hours/6],[assertions]).

% :- trust pred committed_hours(Month,Year,Person,Teaching,Other,Vacation) 
% 	=> ground([Month,Year,Person,Teaching,Other,Vacation]).

:- doc(committed_hours(Month,Year,Person,Teaching,Other,Vacation),
"@var{Teaching},@var{Other}, and @var{Vacation} are the number of
hours which @var{Person} devotes to such activities during
@var{Month}, @var{Year}").

committed_hours(jan,02,german, 0,2,0).
%
committed_hours(_,_,jorge, 9,1,41):-!. %suffices for the time being

