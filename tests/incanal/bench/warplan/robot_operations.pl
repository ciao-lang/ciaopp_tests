:- module(_, [add/2, del/2, moved/2, can/2], []).

add( at(robot,P),       goto1(P,_)).
add( nextto(robot,X),   goto2(X,_)).
add( nextto(X,Y),       pushto(X,Y,_)).
add( nextto(Y,X),       pushto(X,Y,_)).
add( status(S,on),      turnon(S)).
add( on(robot,B),       climbon(B)).
add( onfloor,           climboff(_)).
add( inroom(robot,R2),  gothru(_,_,R2)).

del( at(X,_),U) :- moved(X,U).
del( nextto(Z,robot),U) :- !, del(nextto(robot,Z),U).
del( nextto(robot,X), pushto(X,_,_)) :- !, fail.
del( nextto(robot,B), climbon(B)) :- !, fail.
del( nextto(robot,B), climboff(B)) :- !, fail.
del( nextto(X,_),U) :- moved(X,U).
del( nextto(_,X),U) :- moved(X,U).
del( on(X,_),U) :- moved(X,U).
del( onfloor,climbon(_)).
del( inroom(robot,_), gothru(_,_,_)).
del( status(S,_), turnon(S)).

moved( robot, goto1(_,_)).
moved( robot, goto2(_,_)).
moved( robot, pushto(_,_,_)).
moved( X, pushto(X,_,_)).
moved( robot, climbon(_)).
moved( robot, climboff(_)).
moved( robot, gothru(_,_,_)).


can( goto1(P,R), '##'(locinroom(P,R), '##'(inroom(robot,R), onfloor))).
can( goto2(X,R), '##'(inroom(X,R), ('##'(inroom(robot,R), onfloor)))).
can( pushto(X,Y,R),
    '##'(pushable(X), '##'(inroom(Y,R), '##'(inroom(X,R), '##'(nextto(robot,X),  onfloor))))).
can( turnon(lightswitch(S)),'##'(on(robot,box(1)), nextto(box(1), lightswitch(S)))).
can( climbon(box(B)), '##'(nextto(robot,box(B)), onfloor)).
can( climboff(box(B)), on(robot,box(B))).
can( gothru(D,R1,R2),
    '##'(connects(D,R1,R2), '##'(inroom(robot,R1), '##'(nextto(robot,D), onfloor)))).
