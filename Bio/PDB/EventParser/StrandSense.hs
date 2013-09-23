-- | Module with enumeration of beta-strand senses.
module Bio.PDB.EventParser.StrandSense(StrandSenseT(Parallel, Antiparallel))
where

-- | Enumeration of beta-strand sense.
data StrandSenseT = Parallel |
                    Antiparallel 
  deriving (Eq, Ord, Show, Read)
