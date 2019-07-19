:- module(misc,[ misc/2 ],[]).

:- use_module(library(lists), [length/2]).
:- use_module(honors, [honor/2]).

% Only one of the rules for misc/2 will be applied; if more than
% one can be used, use 'bagof' in the call and sum the results.
% A challenge for OPAL: how to code this, w/o metacall...
% This version is using the first version that succeeds.

% 1. singleton ace, subtract a point (worth 5, not 6); singleton
%    king is worth 2, doubleton queen or jack is worthless.

misc([ace],-1).
misc([king],-2).
misc([queen,_],-2).                             % take off 3 for [queen,jack] ?
misc([jack,_],-1).

% 2. long suits -- 6 cards headed by 3 honors, add 1 for
%    each extra card.

misc([_X,_Y,H3|Rem],P) :-
    honor(H3,X) ,
    X > 0 ,
    length(Rem,N) ,
    N > 2 ,
    P is N-2.

% no rules apply....

misc(_,0).

