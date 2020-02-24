:- module(_, [attribute/2, example/2], []).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% The universe to be clustered.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attribute(epoca, [antigua, media, actual, futura, nula]).
attribute(tema, [ciencia, vida, aventuras, amor, guerra, poesia, religion,
    muerte, ficcion, sexo]).
attribute(autor, [anonimo, antiguo, moderno, medio]).
attribute(publico, [joven, adulto, culto, no_culto]).

example(jazaro, [epoca-[antigua], tema-[ficcion], autor-[moderno],
    publico-[culto]]). 
example(sodoma, [epoca-[actual], tema-[sexo], autor-[moderno],
    publico-[adulto]]).
example(biblia, [epoca-[antigua], tema-[religion], autor-[anonimo, antiguo],
    publico-[joven, adulto]]). 
example(quijote, [epoca-[media], tema-[aventuras, vida, ficcion],
    autor-[medio], publico-[joven, adulto]]).
example(perfume, [epoca-[actual], tema-[ficcion], autor-[moderno],
    publico-[adulto, culto]]).
example(jazmin, [epoca-[actual, media], tema-[amor], autor-[moderno],
    publico-[no_culto]]).
example(dragon, [epoca-[nula], tema-[ciencia], autor-[moderno],
    publico-[culto]]).
example(flores_mal, [epoca-[actual], tema-[amor, muerte, vida],
    autor-[moderno], publico-[adulto, culto]]).
example(maldoror, [epoca-[actual], tema-[vida, muerte], autor-[moderno],
    publico-[adulto, culto]]).
example(fundacion, [epoca-[futura], tema-[ciencia, ficcion], autor-[moderno],
    publico-[joven, adulto]]).
example(neuromante, [epoca-[futura], tema-[ciencia, ficcion], autor-[moderno],
    publico-[joven, adulto]]).
example(lolita, [epoca-[actual], tema-[sexo, amor], autor-[moderno],
    publico-[adulto, culto]]).
example(godel, [epoca-[nula], tema-[ciencia], autor-[moderno],
    publico-[joven, culto]]).
example(nicomaco, [epoca-[nula], tema-[ciencia], autor-[antiguo],
    publico-[adulto, culto]]).

