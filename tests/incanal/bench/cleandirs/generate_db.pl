:- module(generate_db, [generates/2, can_generate_file/2], []).
 %% generate(S1, S2): I can use <file>S1 to generate <file>S2, and
 %% either <file>S1 is smaller (and thus it is preferred), or <file>S1
 %% cannot be generated from <file>S2.
 %% Sometimes <file>S1 and <file>S2 can be generated from each other,
 %% and the sizes are comparable.  I have both options, in that case.

generates('.c', '.o').
generates('.adb', '.o').
generates('.ads', '.ali').
generates('.pl', '.po').
generates('', '.tar').
generates('.tar', '').
generates('.ltx', '.dvi').
generates('.tex', '.dvi').
generates('.tex', '.pdf').
generates('.ps', '.pdf').
generates('.tex', '.html').
generates('.dvi', '.ps').
generates('.tgz', '.tar').
generates('.zip', '').
generates('.gz', '').

 %% And I can chain the steps above: use a .tex to generate a .dvi,
 %% then compress the .tex into a .tex.gz , for example.  I am just
 %% chaining two levels: probably this is enough.

generate_file(FileX, FileY):-
    generates(X, Y),
    atom_concat(Base, Y, FileY),
    atom_concat(Base, X, FileX).

can_generate_file(X, Y):-
    can_generate(X, Y),
    X \== Y.

can_generate(FileX, FileY):-
    generate_file(FileX, FileY).
can_generate(FileX, FileZ):-
    generate_file(FileY, FileZ),
    generate_file(FileX, FileY).
