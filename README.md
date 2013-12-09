hPDB
====
Haskell PDB file format parser.

[![Build Status](https://api.travis-ci.org/BioHaskell/hPDB.png?branch=master)](https://travis-ci.org/BioHaskell/hPDB)

Protein Data Bank file format is a most popular format for holding biomolecule data.

This is a very fast parser:

 - below 7s for the largest entry in PDB - 1HTQ which is over 70MB
 - as compared with 11s of RASMOL 2.7.5,
 - or 2m15s of BioPython with Python 2.6 interpreter.

It is aimed to not only deliver event-based interface, but also a high-level data structure for manipulating data in spirit of BioPython's PDB parser. 

Details on official releases are on [Hackage](http://hackage.haskell.org/package/hPDB).

This package is also a part of [Stackage](http://daniel-diaz.github.io/stackagelist/) - a stable subset of Hackage.
