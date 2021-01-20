:- module(_, [], [doccfg]).

syntax_highlight := yes.

:- include(ciaopp_docsrc(docpaths)).

output_name := 'ciaopp_tests'.

filepath := 'tests/incanal'.

doc_structure := 'incanal_intermod_bench_driver.pl'.

commonopts := no_patches|no_bugs.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

