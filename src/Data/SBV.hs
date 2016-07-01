{-# LANGUAGE FlexibleInstances #-}

module Data.SBV (
    SBool
  , SWord8, SWord16, SWord32, SWord64
  , SInt8, SInt16, SInt32, SInt64
  , SInteger
  , SFloat, SDouble, IEEEFloating(..), IEEEFloatConvertable(..), RoundingMode(..), SRoundingMode, nan, infinity, sNaN, sInfinity
  , sRoundNearestTiesToEven, sRoundNearestTiesToAway, sRoundTowardPositive, sRoundTowardNegative, sRoundTowardZero, sRNE, sRNA, sRTP, sRTN, sRTZ
  , sFloatAsSWord32, sWord32AsSFloat, sDoubleAsSWord64, sWord64AsSDouble, blastSFloat, blastSDouble
  , SReal, AlgReal, sRealToSInteger
  , sBool, sWord8, sWord16, sWord32, sWord64, sInt8, sInt16, sInt32, sInt64, sInteger, sReal, sFloat, sDouble
  , sBools, sWord8s, sWord16s, sWord32s, sWord64s, sInt8s, sInt16s, sInt32s, sInt64s, sIntegers, sReals, sFloats, sDoubles
  , SBV
  , SymArray(..), SArray, SFunArray, mkSFunArray
  , sTestBit, sExtractBits, sPopCount, sShiftLeft, sShiftRight, sRotateLeft, sRotateRight, sSignedShiftArithRight, sFromIntegral, setBitTo, oneIf
  , lsb, msb, label
  , fullAdder, fullMultiplier
  , Splittable(..)
  , Mergeable(..), ite, iteLazy
  , EqSymbolic(..)
  , OrdSymbolic(..)
  , SIntegral
  , SDivisible(..)
  , Boolean(..)
  , bAnd, bOr, bAny, bAll
  , PrettyNum(..), readBin
  , Uninterpreted(..), addAxiom
  , sAssert, SExecutable(..)

  , Symbolic, output, SymWord(..)

  , SBVCodeGen

  , cgPerformRTCs, cgSetDriverValues, cgGenerateDriver, cgGenerateMakefile
  , cgInput, cgInputArr
  , cgOutput, cgOutputArr
  , cgReturn, cgReturnArr
  , cgAddPrototype, cgAddDecl, cgAddLDFlags, cgIgnoreSAssert
  , cgIntegerSize, cgSRealType, CgSRealType(..)
  , compileToC, compileToCLib
  , module Data.Bits
  , module Data.Word
  , module Data.Int
  ) where

import Data.Bits
import Data.Int
import Data.SBV.BitVectors.AlgReals
import Data.SBV.BitVectors.Data
import Data.SBV.BitVectors.Floating
import Data.SBV.BitVectors.Model
import Data.SBV.BitVectors.PrettyNum
import Data.SBV.BitVectors.Splittable
import Data.SBV.Compilers.C
import Data.SBV.Compilers.CodeGen
import Data.SBV.Utils.Boolean
import Data.Word
