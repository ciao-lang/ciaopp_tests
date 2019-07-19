:- module(responsible_persons_upm, [responsible/6], [assertions]).

:- doc( responsible(Project,WP,ID,Start,End,MM1,MM2,MM3,MM4),
   "@var{WP} is the workpackage number within @var{Project}. @var{ID}
   is the task identifier within the @var{WP}. @var{Start} is the
   starting month of the task. The first month of the project is month
   0 and the last month in a 3 year project is month 35. @var{End} is
   the month in which the work in the task is suppossed to
   finish. @var{MMX} is the number of MM which partner @var{X} should
   devote to the task.").

%:- trust pred responsible(A,B,C,D,E,F) => ground([A,B,C,D,E,F]).

responsible(asap,1,1,1,[german,herme],[0.8,0.2]).
responsible(asap,1,2,1,[german,herme,astrid],[0.4,0.4,0.2]).

responsible(asap,4,3,1,[german],[1]).
