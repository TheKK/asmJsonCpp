{-# LANGUAGE OverloadedStrings #-}

module AsmJsonCpp.Asm
  ( AsmObj (..),
    AsmArray (..),
    AsmJson (..),
  )
where

import qualified Data.Text.Lazy as L
import RIO

data AsmObj
  = AtField L.Text AsmJson
  | -- | This constructs product type out from an Object value.
    --
    -- First argument is name of struct. Second argument are name and 'AsmObj' of
    -- fields.
    FieldsToStruct L.Text [(L.Text, AsmJson)]
  deriving (Show)

data AsmArray
  = EachElement AsmJson
  | AtNth Int AsmJson
  | IndexesToStruct L.Text [(Int, L.Text, AsmJson)]
  deriving (Show)

data AsmJson
  = AsInt
  | AsString
  | AsObj AsmObj
  | AsArray AsmArray
  deriving (Show)
