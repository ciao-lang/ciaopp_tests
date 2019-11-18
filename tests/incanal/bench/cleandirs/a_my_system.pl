:- module(_,  [
    modif_time/2,
    datime/9,
    working_directory/2,
    system/1,
    delete_file/1,
    cd/1,
    directory_files/2,
    file_exists/1,
    file_property/2
          ],
    [assertions, isomodes]).

:- use_module(engine(runtime_control), [set_prolog_flag/2, prolog_flag/3]).

:- pred modif_time(+atm, ?int).
modif_time(Path, Time) :-
    prolog_flag(fileerrors, OldFE, off),
    ( file_properties(Path, [], [], Time, [], []) ->
        set_prolog_flag(fileerrors, OldFE)
    ; set_prolog_flag(fileerrors, OldFE),
      fail
    ).

:- trust pred datime(+int,?int,?int,?int,?int,?int,?int,?int,?int)
    # "If @var{Time} is given, the rest of the arguments are unified
    with the date and time to which the @var{Time} argument refers.".

:- trust pred datime(?int,+int,+int,+int,+int,+int,+int,?int,?int) #
    "Bound @var{Time}, @var{WeekDay} and @var{YearDay} as
    determined by the input arguments.".

:- trust pred datime(-int,-int,-int,-int,-int,-int,-int,?int,?int)
    # "Bound @var{Time} to current time and the rest of the
    arguments refer to current time.".

:- impl_defined(datime/9).

:- doc(working_directory(OldDir, NewDir),"Unifies current working
     directory with @var{OldDir}, and then changes the working
     directory to @var{NewDir}. Calling
     @tt{working_directory(Dir,Dir)} simply unifies @tt{Dir} with the
     current working directory without changing anything else.").

:- trust pred working_directory(?atm, +atm) # "Changes current working directory.".
:- trust pred working_directory(OldDir, NewDir)
     : (var(OldDir), var(NewDir), OldDir == NewDir) => atm * atm
     # "Gets current working directory.".
:- impl_defined(working_directory/2).

:- doc(cd(Path), "Changes working directory to @var{Path}.").

:- pred cd(+atm).

cd(Dir) :- working_directory(_, Dir).

% ---------------------------------------------------------------------------

:- doc(directory_files(Directory, FileList), "@var{FileList} is
   the unordered list of entries (files, directories, etc.) in
   @var{Directory}.").

:- trust pred directory_files(+atm,?list(atm)).
:- impl_defined(directory_files/2).


:- doc(system(Command), "Like @pred{shell/1} but ignores exit code.").
:- pred system(+atm).

system(Path) :- shell(Path, _RetCode).

:- pred shell(+atm).

shell(Path) :- shell(Path, 0).

:- doc(shell(Command, RetCode), "Executes @var{Command} using the
   OS-specific system shell and stores the exit code in
   @var{RetCode}.

   On POSIX-like systems the system shell is specified by the
   @tt{SHELL} environment variable (@tt{$SHELL -c \"command\"} for
   passing user commands). On Windows (native builds, MinGW) it is
   specified by the @tt{COMSPEC} environment variable (@tt{%COMSPEC%
   /s /c \"command\"} for passing user commands).

   Note that the use of @pred{shell/2} predicates is discouraged for
   portability and security reasons. Please consider @lib{process} for
   a more robust way to launch external processes.").

:- trust pred shell(+atm, ?int).
:- impl_defined(shell/2).

:- doc(delete_file(File), "Delete the file @var{File}.").

:- trust pred delete_file(+atm).
:- impl_defined(delete_file/1).

:- doc(file_exists(File), "Succeeds if @var{File} (a file or
    directory) exists (and is accessible).").

:- pred file_exists/1: atm.

file_exists(Path) :- file_exists(Path, 0).

:- doc(file_exists(File, Mode), "@var{File} (a file or directory)
   exists and it is accessible with @var{Mode}, as in the Unix call
   @tt{access(2)}. Typically, @var{Mode} is 4 for read permission, 2
   for write permission and 1 for execute permission.").

:- trust pred file_exists(+atm, +int) => atm * int.
:- impl_defined(file_exists/2).

:- pred file_property(+atm, ?struct).

file_property(Path, Property) :-
    file_property_(Property, Path).

file_property_(Property, Path) :-
    var(Property), !,
    file_properties(Path, Type, Linkto, Time, Protection, Size),
    ( Property = type(Type)
    ; Linkto \== '', Property = linkto(Linkto)
    ; Property = mod_time(Time)
    ; Property = mode(Protection)
    ; Property = size(Size)
    ).
file_property_(type(Type), Path) :- !,
    file_properties(Path, Type0, [], [], [], []),
    Type = Type0.
file_property_(linkto(File), Path) :- !,
    file_properties(Path, [], File0, [], [], []),
    File0 \== '',
    File = File0.
file_property_(mod_time(Time), Path) :- !,
    file_properties(Path, [], [], Time, [], []).
file_property_(mode(Protection), Path) :- !,
    file_properties(Path, [], [], [], Protection, []).
file_property_(size(Size), Path) :- !,
    file_properties(Path, [], [], [], [], Size).
file_property_(Other, _) :-
    throw(error(domain_error(file_property_type,Other),
    file_property/2-2)).

:- doc(file_properties(Path, Type, Linkto, Time, Protection, Size),
    "The file @var{Path} has the following properties:

@begin{itemize} 

@item File type @var{Type} (one of @tt{regular}, @tt{directory},
      @tt{fifo}, @tt{socket} or @tt{unknown}).

@item If @var{Path} is a symbolic link, @var{Linkto} is the file pointed
      to.  All other properties come from the file pointed, not the
      link.  @var{Linkto} is '' if @var{Path} is not a symbolic link.

@item Time of last modification @var{Time} (seconds since January, 1,
      1970).

@item Protection mode @var{Protection}.

@item Size in bytes @var{Size}.

@end{itemize}
").

:- trust pred file_properties(+atm, ?atm, ?atm, ?int, ?int, ?int).
:- impl_defined(file_properties/6).
