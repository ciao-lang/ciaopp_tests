:- module(honors,[ honors/3, honor/2 ],[]).

/*
    Count the honors in a suit.  The suit is ordered, so we can stop as
    soon as we hit a low card.  The first clause catches void suits and
    cases when a suit has only honors.
*/

honors([],Pi,Pi).
honors([C1|Cn],Pi,Po) :-
    honor(C1,P) ,
    P > 0 ,
    Pt is P+Pi ,
    honors(Cn,Pt,Po).
honors([C1|_Cn],Pi,Pi) :-
    honor(C1,0).                                % honor(C) = 0 if C not an honor

honor(C,N) :- face_card(C,N).
honor(C,0) :- integer(C).

face_card(ace,4).
face_card(king,3).
face_card(queen,2).
face_card(jack,1).
