{-# LANGUAGE OverloadedStrings #-}

module AsmJsonCpp
  ( AsmObj (..),
    AsmArray (..),
    AsmJson (..)
    )
where

import qualified Data.Text.Lazy as L
import RIO

data AsmObj
  = AtField L.Text AsmJson
  deriving (Show)

data AsmArray
  = EachElement AsmJson
  | AtNth Int AsmJson
  deriving (Show)

data AsmJson
  = AsInt
  | AsString
  | AsObj AsmObj
  | AsArray AsmArray
  deriving (Show)
