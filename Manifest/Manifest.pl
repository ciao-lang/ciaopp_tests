:- bundle(ciaopp_tests).
depends([
  core,
  ciaopp,
  ciaopp_extra
]).

alias_paths([
    ciaopp_tests = 'tests'
]).

manual('ciaopp_tests', [main='doc/SETTINGS.pl']).

cmd('cmds/ciaopp-test').