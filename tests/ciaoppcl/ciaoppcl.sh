#!/bin/bash

_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
                                   cd "$d";done;cd "$(dirname "$e")";pwd -P) || exit

pushd $_base

exampledir=../../../ciaopp_extra/examples/ACC/
outfile=/tmp/ciaopp-output.out

# Simple command line examples

ciaopp -A "$exampledir"/ann.pl -ftypes=none -fmodes=gr -foutput=off &> "$outfile"
cat $outfile
grep "(gr)" "$outfile" || exit

ciaopp -A "$exampledir"/ann.pl -fana_det=det -foutput=off &> "$outfile"
cat $outfile
grep "(det)" "$outfile" || exit

ciaopp -V "$exampledir"/ann.pl -ftypes=none -fmodes=gr -foutput=off &> "$outfile"
cat $outfile
grep "(gr)" "$outfile" || exit

ciaopp -V "$exampledir"/ann.pl -ftypes=terms -fmodes=none -foutput=off &> "$outfile"
cat $outfile
grep "(terms)" "$outfile" || exit

popd
