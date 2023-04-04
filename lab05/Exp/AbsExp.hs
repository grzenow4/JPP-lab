-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Exp.

module Exp.AbsExp where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Exp
    = ELet Ident Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EInt Integer
    | EVar Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

