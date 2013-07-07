#!/bin/bash

FILEPATH=${1:-/media/backup/pdb_flat/pdb1n9j.ent.gz}
UNFILE=`basename $FILEPATH .gz`
EXECUTABLE=../PrintEvents

script() {
  UNFILE=$1
  $EXECUTABLE --unhandled $UNFILE > `basename $UNFILE .ent`.log 2> `basename $UNFILE .ent`.err
}

gzip -c -d $FILEPATH > $UNFILE && (time script $UNFILE) &> `basename $UNFILE .ent`.time && rm -f $UNFILE

# Making uniq twice is much faster (since sort must look at all input before giving output.)
cut -c-6 `basename $UNFILE .ent`.log | uniq | sort | uniq > `basename $UNFILE .ent`.rec

