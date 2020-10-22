:- module(_,[main/1],[assertions]).

:- doc(title, "CiaoPP (incremental) analysis testing and benchmarking command line").
:- doc(author, "Isabel Garcia-Contreras").

:- doc(module,"
Usage:
  @includedef{usage_text/1}.").

:- use_module(library(errhandle)).
:- use_module(library(system)).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(pathnames)).
:- use_module(library(format)).

:- use_module(ciaopp_tests(incanal/incanal_intermod_bench_driver)).

main(['-h']) :- !,
    usage_text(T),
    format(user_output,"Usage: ~s~n",[T]).
main(['new-config',CT,DstFile]) :-
    ( CT = bundle -> Template = 'bundle_config_template.pl'
    ; CT = experiment -> Template = 'experiment_config_template.pl'
    ; fail
    ), !,
    ( file_exists(DstFile) ->
        format(user_error, "ERROR: ~w file exists~n",[DstFile])
    ;
        bundle_path(ciaopp_tests,'tests/incanal/', Dir),
        path_concat(Dir,Template,SrcFile),
        error_protect(copy_file(SrcFile,DstFile),fail)
    ).
main([incanal|Args]) :- !, %% call executable? --> faster load of this command
    incanal_intermod_bench_driver:main(Args).
main(_) :-
    format(user_error, "Wrong arguments. Run with '-h' to show help~n",[]).

usage_text("ciaopp-test <action> [<opts>]

where the possible options are:

  -h: Print this information

  new-config <bundle|experiment> <config-file.pl>
      in <config-file.pl> the template for bundle or experiment configuration will be written

  incanal [incanal args]
").
