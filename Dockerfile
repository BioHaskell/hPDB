FROM afriel/ghc-head-dynamic:latest
RUN sudo apt-get install -y zlib1g-dev
CMD cabal install
