-- | Front-end module presenting minimal interface for serial and parallel parsing.
module Bio.PDB.StructureBuilder(parse
                               ,parseSerial
                               ,parseParallel
                               ,parseWithNParallel)
where

import Bio.PDB.StructureBuilder.Internals
import Bio.PDB.StructureBuilder.Parallel
-- For type declaration:
import Prelude hiding(String)
import Bio.PDB.Common(String)
import Bio.PDB.Structure.List(List(..))
import Bio.PDB.Structure(Structure)
import Bio.PDB.EventParser.PDBEvents(PDBEvent(PDBParseError))

-- | Default parser - uses parallel capabilities, if available.
parse :: FilePath -> String -> (Structure,
                                List PDBEvent)
parse = parseParallel
