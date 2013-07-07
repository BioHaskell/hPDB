#!/usr/bin/python

import sys
import Bio.PDB.PDBParser
import procinfo

if __name__ == "__main__":
  print "argv", sys.argv
  parser    = Bio.PDB.PDBParser(PERMISSIVE=1)
  structure = parser.get_structure(sys.argv[1],
                                   sys.argv[1])
  procinfo.check_memory_use("Memory use after BioPython parsing:",
                            suffixes=False)
