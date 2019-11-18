:- module(test, [test1/0,test2/0,test3/0, test4/0], []).

%------------------------------------------------------------------------------
%       Benchmark Program - (war) plan for robot control
%
%       by D.H.D Warren
%       Date: 
%
%       To test: try test1. test2. test3. or test4.
%------------------------------------------------------------------------------

:- use_module(warplan, [plans/2]).

test1 :-
    plans( status(lightswitch(1),on), start).

test2 :-
    plans( '##'(nextto(box(1),box(2)), nextto(box(2),box(3))), start).

test3 :-
    plans( at(robot,point(6)), start).

test4 :-
    plans('##'(nextto(box(2),box(3)),
            '##'(nextto(box(3),door(1)),
               '##'(status(lightswitch(1),on),
               '##'(nextto(box(1),box(2)), inroom(robot,room(2)))))),
          start).
