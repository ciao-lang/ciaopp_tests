:- module(git_wrapper,[],[assertions,datafacts,isomodes]).

:- doc(title, "Git repository manager").
:- doc(author, "Isabel Garcia-Contreras").

:- doc(module, "Small manager for repositories with git. It contains
	        predicates to:
	
	        @begin{itemize}
	        @item @pred{load_git_repo/2}: Loads a repo (asserts its history with
	                                    commit ids)
	        @item @pred{git_diff_previous/3}: Gets which files change from one
	        commit with respect to the previous one
	        @item @pred{git_checkout/2}: Checks out a version of the repository.
	        @end{itemize}
      ").
	
:- reexport(library(vcs/vcs_git), [git_repo_at_dir/1]).
	
:- use_module(library(vcs/vcs_git)).
:- use_module(library(system), [file_exists/1, working_directory/2]).
:- use_module(library(streams), [fixed_absolute_file_name/3]).
:- use_module(library(messages), [error_message/2]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(process), [process_pipe/2]).
:- use_module(library(bundle/bundle_paths), [reverse_bundle_path/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, "Library database").

% Oldest commits are stored before
:- export(git_log/3).
:- data git_log/3. % git_log(repo,N,commit).

:- doc(git_repo(Id,Path),
        "The git repository at @var{Path} is stored with @var{Id}.").
:- data git_repo/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(section, "Git management predicates").

:- export(load_git_repo/2).
% git log --pretty=format:%h
:- pred load_git_repo(+Path,+RepoId)
        #"Executes the command @code{git log --pretty=format:%h}".
load_git_repo(Path,RepoId) :-
        retractall_fact(git_log(RepoId,_,_)),
        retractall_fact(git_repo(RepoId,_)),
        working_directory(W,W),
        fixed_absolute_file_name(Path,W,APath), %% TODO: this is probably wrong
        ( \+ file_exists(APath) ->
            error_message("~w does not exist", [APath])
        ; ( git_repo_at_dir(APath) ->
              true
	        ;
	            error_message("~w not a git repo", [APath])
          )
        ),
        assertz_fact(git_repo(RepoId,APath)),
        git_loc_option(RepoId,GitLocOption),
        git_output(W, [GitLocOption,log, '--pretty=format:%h',master], History),
        atom_codes(History, HistoryL),
        assert_history(HistoryL,RepoId,0).
	
git_loc_option(RepoId,GitLocOption) :-
        git_repo(RepoId,Path),
        path_concat(Path, '.git', GPath),
        atom_concat('--git-dir=', GPath, GitLocOption).

assert_history([],_, _) :- !.
assert_history(Cs, RepoId, N1) :-
        next_line(Cs,C,RestCs),
        asserta_fact(git_log(RepoId,N1,C)),
        N is N1 + 1,
        assert_history(RestCs, RepoId, N).

next_line(Cs,C,RestCs) :-
        next_line_(Cs,LC,RestCs),
        atom_codes(C,LC).

next_line_([], [], []).
next_line_([0'\n|T], [], T) :- !.
next_line_([C|T], [C|Commit], RT) :-
        next_line_(T, Commit, RT).

:- export(git_diff_previous/3).
% git diff --name-only SHA1 SHA2 % --> and filter .c (only .pl)?
:- pred git_diff_previous(+RepoId,+C,-Files)
        #"Executes the command @code{git diff --name-only SHA1 SHA2_previous}".
git_diff_previous(RepoId,C,Files) :-
        git_loc_option(RepoId,GitLocOption),
        git_log(RepoId, N, C),
        N1 is N - 1,
        git_log(RepoId, N1, C1), !,
        working_directory(W,W),
        git_output(W, [GitLocOption, diff, '--name-only', C, C1], O),
        atom_codes(O,FL),
        parse_files(FL,Files).
git_diff_previous(_,_,[]). % First commit 

parse_files(FS, Fs) :-
        parse_files_(FS, Fs).

parse_files_([],[]) :- !.
parse_files_(Fs,[F|Files]) :-
        next_line(Fs,F,RestFs),
        parse_files_(RestFs,Files).

:- export(git_checkout/3).
:- pred git_checkout(+RepoId, +DstDir, +CommitId).
git_checkout(RepoId, _DstDir, CommitId) :-
        git_loc_option(RepoId,GitLocOption),
        working_directory(W,W),
        git_output(W, [GitLocOption,checkout,'--force',CommitId], _).

:- export(git_checkout_copy/3).
git_checkout_copy(RepoId, DstDir, CommitId) :-
        git_loc_option(RepoId,GitLocOption),
        Args = [GitLocOption, archive, CommitId],
        process_pipe([process_call(path(git), Args),
                      process_call(path(tar), ['-xf', '-'])],
                      [cwd(DstDir),status(0)]).