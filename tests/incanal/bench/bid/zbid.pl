
:- module(zbid,[ bid/4 ],[ assertions, nativeprops]).

:- use_module(evaluate, [evaluate/3]).
:- use_module(sort_hand, [sort_hand/2]).

/*  bid.pl -- compute opening bid for bridge hand

    This program uses rules set forth in Jacoby (1988) counting
    high-card points, distribution points, and a few heuristics
    like support in 10s and 9s and long suits.

    Method -- find points for each suit independently, sum them,
    then adjust for combinations.  Within a suit, compute high-
    card points, then distribution, then adjust for special
    combinations.

    Feb 1991 -- so far the program only counts points; we need
    a bridge expert to add rules for make_bid/4.

    Written By: John Conery
*/

%:- entry bid(X,Y,Z,W)
%	: ( ground(X)
%	  , mshare([[Y],[Z],[W]])
%	  , var(Y), var(Z), var(W)
%         ).

goal(Attr,Pts,Bid) :-
	bid([ace-clubs, 3-hearts, 4-spades, 4-diamonds,
             king-clubs, jack-hearts, ace-spades, ace-hearts,
             10-clubs, 9-clubs, 6-clubs, queen-diamonds, king-spades],
        Attr,Pts,Bid).

bid(Hand,Attributes,Points,Bid) :-
    sort_hand(Hand,SortedHand) ,
    evaluate(SortedHand,Attributes,Points) ,
    make_bid(SortedHand,Attributes,Points,Bid).

make_bid(_Hand,_Attributes,_Points,'punt...').

% utilities


