:- module(sort_hand,[ sort_hand/2 ],[]).

:- use_module(honors, [honor/2]).
:- use_module(filter, [filter/3]).

% Sorting a hand.  Sort into suits, and within a suit, sort
% cards into descending order.  Suits are always returned with
% majors first, minors last.

sort_hand(Hand,SortedHand) :-
    split_suits(Hand,SplitHand) ,
    sort_suits(SplitHand,SortedHand).

% Here's an interesting tradeoff.  In Prolog, the output from
% filter should be two lists, one containing the items that pass
% the test and one containing all the other items.  Later calls
% to filter will be faster, since the input lists get shorter,
% and one fewer call is needed (the output of the second to last
% call has all the clubs).  However, that forces sequential
% execution, since the output of one call is input to the next.

%% clause 27

split_suits(Hand,[spades=S,hearts=H,diamonds=D,clubs=C]) :-
    filter(Hand,spades,S) ,
    filter(Hand,hearts,H) ,
    filter(Hand,diamonds,D) ,
    filter(Hand,clubs,C).

sort_suits([],[]).
sort_suits([S=Ci|In],[S=Co|Out]) :-
    i_sort(Ci,[],Co) ,
    sort_suits(In,Out).

% Sort a list of cards, using a simple insertion sort (the lists
% are likely shorter than 6 cards).

i_sort([],L,L).
i_sort([C1|Cn],Li,Lo) :-
    insert(C1,Li,Lt) ,
    i_sort(Cn,Lt,Lo).

insert(X,[],[X]).
insert(X,[Y|Z],[X,Y|Z]) :- higher(X,Y).
insert(X,[Y|Z],[Y|L])   :- lower(X,Y) , insert(X,Z,L).

% higher(X,Y) -- the rank of card X is greater than or equal to the
%    rank of card Y.
% lower(X,Y) -- the rank of card X is strictly less than the rank of
%    card Y.

higher(ace,_).
higher(king,X)  :- X \== ace.
higher(queen,X) :- X \== ace , X \== king.
higher(jack,X)  :- integer(X).
higher(jack,jack).
higher(X,Y)     :- integer(X) , integer(Y) , X >= Y.

lower(king,ace).
lower(queen,X)  :- honor(X,N) , N > 2.
lower(jack,X)   :- honor(X,N) , N > 1.
lower(X,Y)      :- integer(X) , honor(Y,_M).
lower(X,Y)      :- integer(X) , integer(Y) , X < Y.

