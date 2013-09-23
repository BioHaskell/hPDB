-- | Front-end module presenting minimal interface for serial and parallel parsing.
module Bio.PDB.StructureBuilder(parse, parseParallel, parseWithNParallel) where

import Bio.PDB.StructureBuilder.Internals
import Bio.PDB.StructureBuilder.Parallel
