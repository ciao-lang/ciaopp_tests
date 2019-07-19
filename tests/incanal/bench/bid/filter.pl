:- module(filter,[ filter/3 ],[]).

filter([],_,[]).
filter([Card-Suit|In],Suit,[Card|Out]) :- filter(In,Suit,Out).
filter([_A-X|In],Suit,Out) :- Suit \== X , filter(In,Suit,Out).
