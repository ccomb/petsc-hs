-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.Petsc.Internal.C2HsGen.GenerateC2Hs
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | Type bindings generator for PETSc/SLEPc
-- Generates plain Haskell code (no c2hs required)
-- Compatible with PETSc 3.24+
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.C2HsGen.GenerateC2Hs where

import Data.Char
import Data.List

-- | SETTINGS

-- | output destination
outFormat :: OutputFormat  -- `FileFmt` or `StdoutFmt`
outFormat = StdoutFmt

-- | output file name
outModuleName :: String
outModuleName = "TypesC2HsGen"

-- | module paths
internalPath :: String
internalPath = dottedPath ["Numerical", "PETSc", "Internal"]

c2hsGenPath :: String
c2hsGenPath = dottedPath ["Numerical", "PETSc", "Internal", "C2HsGen"]



-- | Type mappings for PETSc 3.24
-- These are the resolved types that would come from c2hs processing

typeSynonyms :: [(String, String)]
typeSynonyms =
  [ ("PetscScalar_", "CDouble")
  , ("PetscReal_", "CDouble")
  , ("PetscInt_", "CInt")
  , ("PetscLogStage_", "CInt")
  ]

-- | PetscBool for PETSc 3.24+ (typedef bool PetscBool)
petscBoolDef :: String
petscBoolDef = unlines
  [ "-- | PetscBool for PETSc 3.24+ (typedef bool PetscBool)"
  , ""
  , "data PetscBool_ = PetscFalse"
  , "                | PetscTrue"
  , "  deriving (Enum, Eq, Show)"
  , ""
  , "type PetscBool = CBool"
  , ""
  , "petscBoolToC :: PetscBool_ -> PetscBool"
  , "petscBoolToC PetscFalse = 0"
  , "petscBoolToC PetscTrue  = 1"
  , ""
  , "petscBoolFromC :: PetscBool -> PetscBool_"
  , "petscBoolFromC 0 = PetscFalse"
  , "petscBoolFromC _ = PetscTrue"
  ]

-- | DMBoundaryType enumeration
dmBoundaryTypeDef :: String
dmBoundaryTypeDef = unlines
  [ "-- | DMBoundaryType enumeration"
  , ""
  , "data DMBoundaryType_ = DmBoundaryNone"
  , "                     | DmBoundaryGhosted"
  , "                     | DmBoundaryMirror"
  , "                     | DmBoundaryPeriodic"
  , "                     | DmBoundaryTwist"
  , "  deriving (Enum, Eq, Show)"
  , ""
  , "type DMBoundaryType = CInt"
  , ""
  , "dmBoundaryTypeToC :: DMBoundaryType_ -> DMBoundaryType"
  , "dmBoundaryTypeToC = CInt . fromIntegral . fromEnum"
  , ""
  , "dmBoundaryTypeFromC :: DMBoundaryType -> DMBoundaryType_"
  , "dmBoundaryTypeFromC = toEnum . fromIntegral"
  ]

-- | DMDAStencilType enumeration
dmdaStencilTypeDef :: String
dmdaStencilTypeDef = unlines
  [ "-- | DMDAStencilType enumeration"
  , ""
  , "data DMDAStencilType_ = DmdaStencilStar"
  , "                      | DmdaStencilBox"
  , "  deriving (Enum, Eq, Show)"
  , ""
  , "type DMDAStencilType = CInt"
  , ""
  , "dmdaStencilTypeToC :: DMDAStencilType_ -> DMDAStencilType"
  , "dmdaStencilTypeToC = CInt . fromIntegral . fromEnum"
  , ""
  , "dmdaStencilTypeFromC :: DMDAStencilType -> DMDAStencilType_"
  , "dmdaStencilTypeFromC = toEnum . fromIntegral"
  ]

-- | TaoLineSearchConvergedReason enumeration
-- Note: This enum has negative values, so we need explicit Enum instance
taoLineSearchReasonDef :: String
taoLineSearchReasonDef = unlines
  [ "-- | TaoLineSearchConvergedReason enumeration"
  , ""
  , "data TaoLineSearchConvergedReason_ = TaolinesearchFailedAscent"
  , "                                   | TaolinesearchFailedBadparameter"
  , "                                   | TaolinesearchFailedInfornan"
  , "                                   | TaolinesearchContinueIterating"
  , "                                   | TaolinesearchSuccess"
  , "                                   | TaolinesearchSuccessUser"
  , "                                   | TaolinesearchHaltedOther"
  , "                                   | TaolinesearchHaltedMaxfcn"
  , "                                   | TaolinesearchHaltedUpperbound"
  , "                                   | TaolinesearchHaltedLowerbound"
  , "                                   | TaolinesearchHaltedRtol"
  , "                                   | TaolinesearchHaltedUser"
  , "  deriving (Eq, Show)"
  , ""
  , "instance Enum TaoLineSearchConvergedReason_ where"
  , "  fromEnum TaolinesearchFailedAscent = -3"
  , "  fromEnum TaolinesearchFailedBadparameter = -2"
  , "  fromEnum TaolinesearchFailedInfornan = -1"
  , "  fromEnum TaolinesearchContinueIterating = 0"
  , "  fromEnum TaolinesearchSuccess = 1"
  , "  fromEnum TaolinesearchSuccessUser = 2"
  , "  fromEnum TaolinesearchHaltedOther = 3"
  , "  fromEnum TaolinesearchHaltedMaxfcn = 4"
  , "  fromEnum TaolinesearchHaltedUpperbound = 5"
  , "  fromEnum TaolinesearchHaltedLowerbound = 6"
  , "  fromEnum TaolinesearchHaltedRtol = 7"
  , "  fromEnum TaolinesearchHaltedUser = 8"
  , ""
  , "  toEnum (-3) = TaolinesearchFailedAscent"
  , "  toEnum (-2) = TaolinesearchFailedBadparameter"
  , "  toEnum (-1) = TaolinesearchFailedInfornan"
  , "  toEnum 0 = TaolinesearchContinueIterating"
  , "  toEnum 1 = TaolinesearchSuccess"
  , "  toEnum 2 = TaolinesearchSuccessUser"
  , "  toEnum 3 = TaolinesearchHaltedOther"
  , "  toEnum 4 = TaolinesearchHaltedMaxfcn"
  , "  toEnum 5 = TaolinesearchHaltedUpperbound"
  , "  toEnum 6 = TaolinesearchHaltedLowerbound"
  , "  toEnum 7 = TaolinesearchHaltedRtol"
  , "  toEnum 8 = TaolinesearchHaltedUser"
  , "  toEnum n = error $ \"TaoLineSearchConvergedReason_.toEnum: Cannot match \" ++ show n"
  , ""
  , "type TaoLineSearchConvergedReason = CInt"
  , ""
  , "taoLineSearchConvergedReasonToC :: TaoLineSearchConvergedReason_ -> TaoLineSearchConvergedReason"
  , "taoLineSearchConvergedReasonToC = CInt . fromIntegral . fromEnum"
  , ""
  , "taoLineSearchConvergedReasonFromC :: TaoLineSearchConvergedReason -> TaoLineSearchConvergedReason_"
  , "taoLineSearchConvergedReasonFromC = toEnum . fromIntegral"
  ]


-- | Generate the complete module

main :: IO ()
main | outFormat == StdoutFmt = putStr moduleContent
     | otherwise = writeFile (outModuleName ++ ".hs") moduleContent

moduleContent :: String
moduleContent = unlines
  [ "{-# LANGUAGE ForeignFunctionInterface #-}"
  , "-- | Generated type bindings for PETSc/SLEPc"
  , "-- Compatible with PETSc 3.24+ where PetscBool is typedef bool (not enum)"
  , "module " ++ c2hsGenPath ++ outModuleName ++ " where"
  , ""
  , "import Foreign"
  , "import Foreign.C.Types"
  , ""
  , "import " ++ internalPath ++ "Utils"
  , ""
  , "-- | Type synonyms"
  , ""
  , typeSynonymDecls
  , ""
  , petscBoolDef
  , ""
  , dmBoundaryTypeDef
  , ""
  , dmdaStencilTypeDef
  , ""
  , taoLineSearchReasonDef
  ]

typeSynonymDecls :: String
typeSynonymDecls = unlines $ map mkTypeSyn typeSynonyms
  where
    mkTypeSyn (hsName, cType) = "type " ++ hsName ++ " = " ++ cType



-- | Types and helpers

data OutputFormat = StdoutFmt | FileFmt deriving (Eq, Show)

dottedPath :: [String] -> String
dottedPath pp = concat $ map (\p -> p ++ ".") pp
