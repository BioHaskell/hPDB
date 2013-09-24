{-# LANGUAGE BangPatterns, DisambiguateRecordFields, MultiParamTypeClasses, NamedFieldPuns, FlexibleContexts, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE RecordWildCards #-} -- for convenient debugging
{-# OPTIONS_GHC -fspec-constr-count=2 #-}
module Bio.PDB.StructureBuilder.Internals
--(parse)

where

import Prelude hiding (String)
import qualified Data.ByteString.Char8 as BS hiding (reverse)
import qualified Control.Monad.ST      as ST
import Control.Monad.State.Strict      as State 
import Control.Monad(when)
import Data.STRef                      as STRef
import Data.Maybe(isNothing, isJust)

import Bio.PDB.EventParser.PDBEvents(PDBEvent(..), RESID(..))
import qualified Bio.PDB.EventParser.PDBEventParser(parsePDBRecords)
import Bio.PDB.Structure
import Bio.PDB.Structure.List as L

-- | Shorthand for the State monad in which parsing is done.
-- `t` is existential 'phantom' type to keep ST effects from escaping
type ParsingMonad t a = State.StateT (BState t) (ST.ST t) a

-- TODO: with option of online reporting of errors?

-- parsePDBRec :: (Monad m) => String -> String -> (() -> PDBEvent -> m ()) -> () -> m ()
-- | Parses PDB records given as ByteString, given filename fileContents and a monadic
-- action to be executed for each PDB event.
parsePDBRec :: String -> String -> (() -> PDBEvent -> ParsingMonad t ()) -> () -> ParsingMonad t ()
parsePDBRec = Bio.PDB.EventParser.PDBEventParser.parsePDBRecords

-- | Given filename, and contents, parses a whole PDB file, returning a monadic action
-- | with a tuple of (Structure, [PDBEvent]), where the list of events contains all
-- | parsing or construction errors.
parseSerial :: FilePath -> String -> (Structure, List PDBEvent)
parseSerial fname contents = ST.runST $ do initial <- initializeState
                                           (s, e)  <- State.evalStateT parsing initial
                                           return (s :: Structure, e :: L.List PDBEvent)
  where parsing = do parsePDBRec (BS.pack fname) contents (\() !ev -> parseStep ev) ()
                     closeStructure
                     s  <- State.gets currentStructure
                     e  <- State.gets errors
                     e' <- L.finalize e
                     return (s, e')

-- | Record holding a current state of the structure record builder.
data BState s = BState { currentResidue    :: Maybe Residue,
                         currentModel      :: Maybe Model,
                         currentChain      :: Maybe Chain,
                         currentStructure  :: Structure,
                         residueContents   :: L.TempList  s Atom,
                         chainContents     :: L.TempList  s Residue,
                         modelContents     :: L.TempList  s Chain,
                         structureContents :: L.TempList  s Model,
                         errors            :: L.TempList  s PDBEvent,
                         lineNo            :: STRef.STRef s Int
                       }

-- | Initial state of the structure record builder.
initializeState :: ST.ST t (BState t)
initializeState = do r  <- L.initialNew L.residueVectorSize 
                     c  <- L.initialNew L.chainVectorSize
                     m  <- L.initialNew 1
                     s  <- L.initialNew 1
                     e  <- L.initialNew 100
                     l  <- STRef.newSTRef 1
                     return BState { currentResidue    = Nothing,
                                     currentModel      = Nothing,
                                     currentChain      = Nothing,
                                     currentStructure  = Structure { models = L.empty },
                                     residueContents   = r,
                                     chainContents     = c,
                                     modelContents     = m,
                                     structureContents = s,
                                     errors            = e,
                                     lineNo            = l }
-- | Checks that a residue with a given identification tuple is current,
-- | or if not, then closes previous residue (if present),
-- | and marks a new ,,current'' residue in a state of builder.
checkResidue :: Bio.PDB.EventParser.PDBEvents.RESID -> ParsingMonad t ()
checkResidue (RESID (newName, newChain, newResseq, newInsCode)) =
  do checkChain newChain
     res <- State.gets currentResidue
     when (residueChanged res) $ do closeResidue
                                    l <- L.new L.residueVectorSize
                                    State.modify $! createResidue l
  where
    residueChanged Nothing = True
    residueChanged (Just (Residue { resName = oldResName,
                                    resSeq  = oldResSeq ,
                                    insCode = oldInsCode,
                                    atoms   = _atoms    })) =
      (oldResName, oldResSeq, oldInsCode) /= (newName, newResseq, newInsCode)
    createResidue l st = st { currentResidue  = Just newResidue,
                              residueContents = l }
    newResidue = Bio.PDB.Structure.Residue { resName = newName,
                                             resSeq  = newResseq,
                                             insCode = newInsCode,
                                             atoms   = L.empty }
        
-- | Checks that a chain with a given identification character is current,
-- | and if not, creates one. Also checks that we have any model in which
-- | to assign the chain.
checkChain :: Char -> ParsingMonad t ()
checkChain name = do checkModel
                     curChain <- State.gets currentChain
                     when (chainChanged curChain) $ do closeChain
                                                       l <- L.new L.chainVectorSize
                                                       State.modify $ createChain l
  where
    chainChanged Nothing                               = True
    chainChanged (Just (Chain { chainId = oldChain })) = oldChain /= name
    createChain l state = state { currentChain  = Just Chain { chainId  = name,
                                                               residues = L.empty },
                                  chainContents = l }


-- | Checks that a current model has been declared, and creates zeroth model,
-- | if no such model exists.
checkModel :: ParsingMonad t ()
checkModel = do curModel <- State.gets currentModel
                when (isNothing curModel) $ openModel defaultModelId
-- | Closes construction of a current residue and appends this residue to a current chain. (Monadic action.)
--closeResidue :: State.State BState ()
-- TODO: when createing a dummy model, check that there are no models declared before
--       [Otherwise one needs to report an error!]

-- | Default model id, in case none was indicated (for comparison.)
defaultModelId = 1

closeResidue :: ParsingMonad t ()
closeResidue = do r <- State.gets currentResidue
                  when (isJust r) $ do let Just res = r
                                       rc  <- State.gets residueContents
                                       rf  <- L.finalize rc
                                       cc  <- State.gets chainContents
                                       cc' <- L.add cc $ res { Bio.PDB.Structure.atoms = rf }
                                       State.modify clearResidue
  where
    clearResidue st = st { currentResidue = Nothing }

-- | Finalizes construction of current chain, and appends it to current model.
--closeChain :: State.State BState ()

closeChain :: ParsingMonad t ()
closeChain = do closeResidue
                c  <- State.gets currentChain
                ac <- State.gets chainContents
                when (isJust c) $ do l   <- State.gets chainContents
                                     l'  <- L.finalize l
                                     let Just ch = c
                                         ch'     = ch { Bio.PDB.Structure.residues = l' }
                                     m   <- State.gets currentModel
                                     when (isNothing m) $ do mli <- State.gets structureContents
                                                             i <- L.tempLength mli
                                                             openModel i
                                                             addError ["Trying to close chain when currentChain is ",
                                                                       BS.pack . show $ ch,
                                                                       " and currentModel is ",
                                                                       BS.pack . show $ m]
                                     ml  <- State.gets modelContents
                                     ml' <- L.add ml ch'
                                     State.modify clearChain
  where
    clearChain st = st { currentChain = Nothing }

-- | Reports error during building of structure for PDB entry.
-- TODO: This should be probably monadic action
-- TODO: forgot about line/column number passing!
addError :: [String] -> ParsingMonad t ()
addError msg = do e  <- State.gets errors
                  lnref <- State.gets lineNo
                  ln <- lift $ STRef.readSTRef lnref
                  lift $ STRef.modifySTRef lnref (+1)
                  L.add e $ anError ln
  where
    anError ln = PDBParseError ln 0 $ BS.concat msg

-- | Finalizes construction of current model
closeModel :: ParsingMonad t ()
closeModel = do closeChain
                cm <- State.gets currentModel
                case cm of
                  Nothing -> return ()
                  Just m  -> do mc  <- State.gets modelContents
                                chs <- L.finalize mc
                                let m' = m { chains = chs }
                                sc  <- State.gets structureContents
                                State.modify clearModel
                                L.add sc m'
  where clearModel st = st { currentModel = Nothing }

-- | Finalizes construction of record holding PDB entry data.
-- NOTE: this one is different and should only be used after parsing is complete!

closeStructure :: ParsingMonad t ()
closeStructure = do closeModel
                    sc  <- State.gets structureContents
                    sc' <- L.finalize sc
                    State.modify (closeStructure' sc')
  where
    closeStructure' sc bstate@(BState { currentStructure = aStructure}) =
      bstate { currentStructure  = aStructure { models = sc },
               structureContents = undefined }

nextLine :: ParsingMonad t ()
nextLine = do lnref <- State.gets lineNo
              lift $ STRef.modifySTRef lnref (+1)

-- | Performs a match on a single PDBEvent and performs relevant change to a BState of structure builder.
--parseStep   :: (State.MonadState BState m) => PDBEvent -> m ()
parseStep pe@(PDBParseError l _ _) = do e  <- State.gets errors
                                        L.add e pe 
                                        lnref <- State.gets lineNo
                                        lift $ STRef.writeSTRef lnref l
parseStep (ATOM { no        = atSer,      -- :: !Int,
                  atomtype  = atType,     -- :: !String,
                  restype   = resName,    -- :: !String,
                  chain     = chainName,  -- :: !Char,
                  resid     = resSeq,     -- :: !Int,
                  resins    = resInsCode, -- :: !Char,
                  altloc    = altloc,     -- :: !Char, - atom name 
                  coords    = atCoord,    -- :: !Vector3,
                  occupancy = atOccupancy,-- :: !Double,
                  bfactor   = atBFactor,  -- :: !Double,
                  segid     = atSegId,    -- :: !String,
                  elt       = atElement,  -- :: !String,
                  charge    = atCharge,   -- :: !String, -- why not a number?
                  hetatm    = isHet       -- :: !Bool
                })            =
  do checkResidue $ RESID (resName, chainName, resSeq, resInsCode)
     reslist <- State.gets residueContents
     newAtom `seq` L.add reslist newAtom
     nextLine
  where newAtom = Atom { atName    = atType,
                         atSerial  = atSer,
                         coord     = atCoord,
                         bFactor   = atBFactor,
                         occupancy = atOccupancy,
                         element   = atElement,
                         segid     = atSegId,
                         charge    = atCharge,
                         hetatm    = isHet
                       }

parseStep (MODEL { num = n }) = do closeModel
                                   openModel n
                                   nextLine
parseStep ENDMDL              = do closeModel
                                   nextLine
parseStep END                 = do closeModel
                                   nextLine
parseStep (TER {..})          = do closeChain -- TODO: check TER with currentChain parameters
                                   nextLine
parseStep (MASTER {..})       = do closeModel -- TODO: check MASTER parameters with current model -- is it really model end?
                                   nextLine
parseStep _                   =    nextLine 

-- | Creates a new model within structure builder. (For internal use.)
-- WARNING: And forgets anything that was there before!
openModel :: Int -> ParsingMonad t () 
openModel n = do l <- L.new L.defaultSize
                 State.modify $ changeModel l
  where changeModel l st = st { currentModel  = Just newModel,
                                modelContents = l }
        newModel  = Bio.PDB.Structure.Model { modelId = n,
                                              chains  = empty }
        
-- | Finalizes state of structure builder, and returns pair of a structure, and list of errors.
-- NOTE: should have a monadic action for each error instead. Then possibly default monad that accumulates these errors.
parseFinish :: ParsingMonad t (Structure, L.List PDBEvent)
parseFinish = do closeStructure
                 st  <- State.gets currentStructure
                 er  <- State.gets errors
                 er' <- finalize er
                 st `seq` return (st, er')

