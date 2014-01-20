#!/bin/bash

#docker run -v `pwd`:`pwd` -w `pwd` afriel/ghc-head-dynamic bash -c "sudo apt-get install -y zlib1g-dev libstdc++-dev; cabal update && cabal install "

docker run -v `pwd`:`pwd` -v `pwd`/dist -w `pwd` afriel/ghc-head-dynamic bash -c "sudo apt-get install -y zlib1g-dev libstdc++-4.8-dev libstdc++6-4.7-dev g++; cabal update && cabal install"
