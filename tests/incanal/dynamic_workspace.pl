:- module(_,[main/1],[fsyntax,datafacts,dynamic,hiord,assertions]). % hiord only for testing

:- doc(title, "Dynamic change of alias paths in bundles").

:- doc(module, "
@section{Usage example}

Create or copy a bundle, for example:
@includedef{write_bundle/1}

Add the path to the bundle to the @tt{CIAOPATH} environment variable and reload:
@includedef{main/1}
").

:- use_module(library(pathnames)).
:- use_module(library(system)).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(compiler)).
:- use_module(ciaobld(bundle_scan), [scan_bundles_at_path/1]).
:- use_module(library(terms), [atom_concat/2]).

% TODO: --- hack to reload bundles when CIAOPATH changes ---
:- multifile library_directory/1.
:- dynamic library_directory/1.
:- multifile file_search_path/2.
:- dynamic file_search_path/2.
:- use_module(engine(internals), []). % DO NOT REMOVE
:- import(internals, [setup_paths/0]).

:- export(reload_ciaopath/0).
reload_ciaopath :-
    retractall_fact(library_directory(_)),
    retractall_fact(file_search_path(_,_)),
    internals:setup_paths.

:- export(activate_workspace/1).
activate_workspace(Wksp) :-
    mkpath(~path_concat_list([Wksp,build])),
    mkpath(~path_concat_list([Wksp,build,cache])),
    set_env('CIAOPATH',~atom_concat([Wksp,':',~current_env('CIAOPATH')])).


:- doc(section, "Usage example").
main([Wksp]) :-
    % Add a new workspace (which may not contain any bundle yet)
    mkpath(Wksp),
    activate_workspace(Wksp),
    reload_ciaopath,
    % Create a new bundle and re-scan bundles at this workspace
    path_concat(Wksp,mybundle,BndlDir),
    write_bundle(BndlDir),
    scan_bundles_at_path(Wksp), % (needed only if bundle is new or
                                % Manifest is changed)
    % Load and call
    use_module(mybundle(source)),
    ( M = source, M:p(X), display(p(X)), nl, fail ; true ).

write_bundle(BndlDir) :-
    % write Manifest.pl
    open(~path_concat(BndlDir,'Manifest.pl'), write, S1),
    write(S1,':- bundle(mybundle).\nalias_paths([mybundle = src]).\n'),
    close(S1),
    % write sources
    mkpath(BndlDir),
    mkpath(~path_concat(BndlDir,src)),
    open(~path_concat_list([BndlDir,src,'source.pl']), write, S2),
    write(S2,':- module(_,[p/1],[]).\np(a).\n'),
    close(S2).