#!/bin/bash

echo "Set PROGS"
PROGS="rasmol jmol ./pymoltest ./PDB2Fasta.sh ./test.rb ./PDBParse.py ./parseBioJava.sh"
# ?molmol raster3d 
# apt-get install jmol rasmol ruby-bio
# gdis garlic 
TEMPOUT=speedtest.out
CSVOUT=speedtest.csv
#TEMPOUT=~/tmp/parsers_w_ruby.out
#OUT=~/tmp/parsers_w_ruby.txt

for PROG in $PROGS ; do
  for STRUCTURE in ../1CRN.pdb ../3JYV.pdb ../1HTQ.pdb ; do
    echo "***** $PROG $STRUCTURE *****";
    echo "PROG=$PROG"
    echo "STRUCTURE=$STRUCTURE"
    time $PROG $STRUCTURE < /dev/null;
  done;
done &> $TEMPOUT
#egrep 'PROG=|STRUCTURE=|^real|^user|^sys|^Memory used|bytes maximum residency|total memory in use|Used memory is bytes: ' $TEMPOUT > $OUT

./speedtest_parse.py $TEMPOUT $CSVOUT

