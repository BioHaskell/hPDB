#!/bin/bash

time (ghc -O3 -threaded -rtsopts test_parallel.hs && time ./test_parallel 3JYV.pdb && time ./test_parallel 3JYV.pdb +RTS -N)
#ghc -O3 -rtsopts test_parallel.hs && time ./test_parallel 1HTQ.pdb
