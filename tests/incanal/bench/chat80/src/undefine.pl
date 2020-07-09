:- module(undefine,
    [ adv_template/4,
      ditrans/12,
      person/1,
      pp_quant/2,
      standard/4
    ],
    []).

:- use_module(engine(messages_basic), [message/2]).

person(_):- undefined(person,1), fail.

pp_quant(_,_):- undefined(pp_quant,2), fail.

standard(_,_,_,_):- undefined(standard,4), fail.

adv_template(_,_,_,_):- undefined(adv_template,4), fail.

ditrans(_,_,_,_,_,_,_,_,_,_,_,_):- undefined(ditrans,12), fail.

undefined(F,A):-
    message(error, [predicate,' ',''(F),/,~~(A),' ',undefined,' - ',failing]).
