hPDB
====
Haskell PDB file format parser.

[![Build Status](https://api.travis-ci.org/BioHaskell/hPDB.svg?branch=master)](https://travis-ci.org/BioHaskell/hPDB)
[![Hackage](https://budueba.com/hackage/hPDB)](https://hackage.haskell.org/package/hPDB)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/hPDB.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=hPDB)

Protein Data Bank file format is a most popular format for holding biomolecule data.

This is a very fast parser:

 - below 7s for the largest entry in PDB - 1HTQ which is over 70MB
 - as compared with 11s of RASMOL 2.7.5,
 - or 2m15s of BioPython with Python 2.6 interpreter.

It is aimed to not only deliver event-based interface, but also a high-level data structure for manipulating data in spirit of BioPython's PDB parser. 

Details on official releases are on [Hackage](https://hackage.haskell.org/package/hPDB)

This package is also a part of [Stackage](http://www.stackage.org/package/hPDB) - a stable subset of Hackage.

Projects for the future:
------------------------

Please let me know if you would be willing to push the project further.

In particular one may considering these features:

* Migrate out of `text-format`, since it gives portability trouble, and slows things down when printing.
* Migrate from `AC-Vector` to another vector library:
    - `vector-space`
    - or `linear`
* Use `lens` to facilitate access to the data structures.
    - torsion angles within protein/RNA chain.
* Add Octree to the default data structure (with automatic update.)
* Write a combinator library for generic fast parsing.
* Checking whether GHC 7.8 improved efficiency of fixed point arithmetic,
since PDB coordinates have dynamic range of just ~2^20 bits, with smallest
step of 0.001.
* Implement basic spatial operations of RMS superposition (with SVD),
affine transform on a substructure.
* Class-based wrappers showing Structure-Model-Chain-Residue-Atom interface
with possible wrapping of Repa/Accelerate arrays for fast computation.

Please ask me any questions on [Gitter](https://gitter.im/mgajda).
