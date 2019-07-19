:- module(workplan, [task/6], [assertions]).

:- doc(module, "This modules contains the basic information
   regarding the taks of @var{Project} as described in the
   contract. Though in principle only one fact of @pred{task/6} is
   needed per task (a pair of @var{WP} and @var{ID}), more than one
   may be introduced if more sophisticated distribution of workload is
   desired. For example, if we are not happy about even distribution
   of load during the lifetime of a task we may split a task into two
   or more subtasks with their corresponding load and start and end
   times.

   It is important to note that the start and end of tasks is given at
   the level of months. The first month of the project is month 0. A
   task which is active during all the life-time of a 3-year project
   has 0 and 36 (not 35!) as start and end dates. This should be
   interpreted as 36 is the first month in which the work in the task
   has finished.").

:- doc( task(Project,WP,ID,Start,End,Name), "@var{WP} is the
     workpackage number within @var{Project}. @var{ID} is the task
     identifier within the @var{WP}. @var{Start} is the starting month
     of the task.  @var{End} is the month in which the work in the
     task is suppossed to finish. @var{Name} is the title of the
     task.").

% :- trust pred task(A,B,C,D,E,F) => ground([A,B,C,D,E,F]).

task(asap,1,1,0,36,"Internal Management").
