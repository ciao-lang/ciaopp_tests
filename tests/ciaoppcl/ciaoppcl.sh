#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P) || exit

pushd $_base

exampledir=../../../ciaopp_extra/examples/ACC/
outfile=/tmp/ciaopp-output.out

# Simple command line examples

ciaopp -V "$exampledir"/ann.pl -ftypes=none -fmodes=gr -fmenu_output=off &> "$outfile"
cat $outfile
grep "(gr)" "$outfile" || exit

ciaopp -V "$exampledir"/ann.pl -ftypes=terms -fmenu_output=off &> "$outfile"
cat $outfile
grep "(terms)" "$outfile" || exit

popd
