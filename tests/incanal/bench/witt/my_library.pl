:- module(_,
	[my_own_ordunion/3,
	 my_own_ordintersection/3,
	 list_to_my_own_ordset/2,
	 my_own_select_/3,
	 my_own_memberchk/2,
	 my_own_append/3,
	 non_my_own_member/2,
	 my_own_member/2], []).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Several public library predicates, from O'Keefe's shared code.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(sort), [sort/2]).

%   my_own_ordunion(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

my_own_ordunion([], Set2, Set2) :- !.
my_own_ordunion(Set1, [], Set1) :- !.
my_own_ordunion([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	my_own_ordunion(Order, Head1, Tail1, Head2, Tail2, Union).

my_own_ordunion(<, Head0, [], Head2, Tail2, [Head0,Head2|Tail2]) :- !.
my_own_ordunion(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Union]) :-
	compare(Order, Head1, Head2),
	my_own_ordunion(Order, Head1, Tail1, Head2, Tail2, Union).
my_own_ordunion(=, Head,  Tail1, _,	  Tail2, [Head|Union]) :-
	my_own_ordunion(Tail1, Tail2, Union).
my_own_ordunion(>, Head1, Tail1, Head0, [], [Head0,Head1|Tail1]) :- !.
my_own_ordunion(>, Head1, Tail1, Head0, [Head2|Tail2], [Head0|Union]) :-
	compare(Order, Head1, Head2),
	my_own_ordunion(Order, Head1, Tail1, Head2, Tail2, Union).



%   my_own_ordintersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

% Not used
my_own_ordintersection([], _, []) :- !.
my_own_ordintersection(_, [], []) :- !.
my_own_ordintersection([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	my_own_ordintersection(Order, Head1, Tail1, Head2, Tail2, Intersection).

my_own_ordintersection(<, _, [], _, _, []) :- !.
my_own_ordintersection(<, _, [Head1|Tail1], Head2, Tail2, Intersection) :-
	compare(Order, Head1, Head2),
	my_own_ordintersection(Order, Head1, Tail1, Head2, Tail2, Intersection).
my_own_ordintersection(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	my_own_ordintersection(Tail1, Tail2, Intersection).
my_own_ordintersection(>, _, _, _, [], []) :- !.
my_own_ordintersection(>, Head1, Tail1, _, [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	my_own_ordintersection(Order, Head1, Tail1, Head2, Tail2, Intersection).



%   list_to_my_own_ordset(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  

list_to_my_own_ordset(List, Set) :-
	sort(List, Set).


 %:- use_module(library(lists)).



%   my_own_select_(?Element, ?List, ?List2)
%   is true when the result of removing an occurrence of Element in List
%   is List2.

my_own_select_(Element, [Element|Tail], Tail).
my_own_select_(Element, [Head|Tail1], [Head|Tail2]) :- 
	my_own_select_(Element, Tail1, Tail2).


%   my_own_member(?Element, +List)
%   is true when Element is a my_own_member of List.  It may be used to test 
%   for my_own_membership in a list, but it can also be used to enumerate all 
%   the elements in List.

my_own_member(Element, [Head|Tail]) :-
	my_own_member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
my_own_member_(_, Element, Element).
my_own_member_([Head|Tail], _, Element) :-
	my_own_member_(Tail, Head, Element).


%   my_own_memberchk(+Element, +List)
%   is true when Element is a my_own_member of List, but my_own_memberchk/2 only succeeds
%   once and can therefore not be used to enumerate the elements in List.

my_own_memberchk(Element, [Element|_]) :- !.
my_own_memberchk(Element, [_|Rest]) :-
	my_own_memberchk(Element, Rest).



%   my_own_append(?Prefix, ?Suffix, ?Combined)
%   is true when Combined is the combined list of the elements in Prefix 
%   followed by the elements in Suffix. It can be used to form Combined or
%   it can be used to find Prefix and/or Suffix from a given Combined  

my_own_append([], List, List).
my_own_append([Head|Tail], List, [Head|Rest]) :- 
	my_own_append(Tail, List, Rest).


%   non_my_own_member(+Element, +List)
%   non_my_own_member is true when Element does not exist in List.

non_my_own_member(Element, List) :-
	non_my_own_member_(List, Element).


non_my_own_member_([], _).
non_my_own_member_([Head|Tail], Element) :-
%	dif(Head, Element),
        Head \== Element,
	non_my_own_member_(Tail, Element).


%% natural logarithm, which treats the special case of a zero as
%% argument. For our convenience, 0 will be returned.
