{-# LANGUAGE OverloadedStrings #-}
-- | This module contains an enumeration of experimental methods.
module Bio.PDB.EventParser.ExperimentalMethods(ExpMethod(..), mkExpMethod, showExpMethod)
where

import qualified Data.ByteString.Char8 as BS

-- | Enumeration of experimental methods occuring in the PDB archive.
data ExpMethod = XRayDiffraction         |
                 FiberDiffraction        |
                 NeutronDiffraction      |
                 ElectronCrystallography |
                 ElectronMicroscopy      |
                 SolidStateNMR           |
                 SolutionNMR             |
                 SolutionScattering      |
                 OtherExpMethod !BS.ByteString
  deriving (Show, Read, Eq, Ord)

-- | Generates an ExpMethod from words in PDB
mkExpMethod :: [BS.ByteString] -> ExpMethod
mkExpMethod ["X-RAY", "DIFFRACTION"]        = XRayDiffraction
mkExpMethod ["FIBER", "DIFFRACTION"]        = FiberDiffraction
mkExpMethod ["NEUTRON", "DIFFRACTION"]      = NeutronDiffraction
mkExpMethod ["ELECTRON", "CRYSTALLOGRAPHY"] = ElectronCrystallography
mkExpMethod ["ELECTRON", "MICROSCOPY"]      = ElectronMicroscopy
mkExpMethod ["SOLID-STATE", "NMR"]          = SolidStateNMR
mkExpMethod ["SOLUTION", "NMR"]             = SolutionNMR
mkExpMethod ["SOLUTION", "SCATTERING"]      = SolutionScattering
mkExpMethod other                           = OtherExpMethod (BS.unwords other) -- error-like

-- | Converts an ExpMethod back into text
showExpMethod :: ExpMethod -> BS.ByteString
showExpMethod XRayDiffraction         = "X-RAY DIFFRACTION"
showExpMethod FiberDiffraction        = "FIBER DIFFRACTION"
showExpMethod NeutronDiffraction      = "NEUTRON DIFFRACTION"
showExpMethod ElectronCrystallography = "ELECTRON CRYSTALLOGRAPHY"
showExpMethod ElectronMicroscopy      = "ELECTRON MICROSCOPY"
showExpMethod SolidStateNMR           = "SOLID-STATE NMR"
showExpMethod SolutionNMR             = "SOLUTION NMR"
showExpMethod SolutionScattering      = "SOLUTION SCATTERING"
showExpMethod (OtherExpMethod other)  = other

