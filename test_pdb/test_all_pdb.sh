#!/bin/bash

PDBPATH=/media/backup/pdb_flat

for PDB in $PDBPATH/*.ent.gz; do
  nice -n 20 ./test_on_pdb.sh $PDB
done;

