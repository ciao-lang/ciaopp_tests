#!/bin/sh

# This script acts as an oracle, identifying differences in ciaopp example outputs.
# Comparison is textual.

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

ciaopp_tutorials_dir="$_base/../../../ciaopp/doc/tutorials/"
res_dir=results
res_dir_old=$res_dir-saved

all_OK=yes

# (Re)generate examples
cd "$ciaopp_tutorials_dir"
mkdir -p "$res_dir_old"
mv "$res_dir"/* "$res_dir_old"

# Clean to ensure regeneration
lpdoc --clean -t html SETTINGS.pl
lpdoc -t html SETTINGS.pl
# Process items at results/
ciao-exfilter

# Iterate through the files in results
for file in "$res_dir"/*.txt; do
    # Get the corresponding file
    file_saved=$res_dir_old/$(basename "$file")
    # Check if the corresponding file in results-saved exists
    if [ -f "$file_saved" ]; then
        if diff -q "$file" "$file_saved"  >/dev/null 2>&1; then
            echo "[OK] $file and $file_saved are the same"
        else
            echo "[??] Files $file and $file_saved differ:"
            diff "$file" "$file_saved"
            all_OK=no
        fi        
    else
        echo "Corresponding file $file_saved not found"
        all_OK=no
    fi
done

if [ $all_OK = yes ]; then
    exit 0
else
    exit 1
fi
