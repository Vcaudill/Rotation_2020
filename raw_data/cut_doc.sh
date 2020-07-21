#!/bin/bash

cut_path="cut_data/"
for f in *; do 
egrep -v "WARNING|Starting|Initial*|initial*|OUT|Mut*|done" ${f} > temp && mv temp ${cut_path}$f
tail -n +6 ${cut_path}$f > "file.tmp" && mv "file.tmp" ${cut_path}$f
echo $f;

done
