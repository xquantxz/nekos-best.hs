module NekosBest.Error (
    NbError(..)
) where

newtype NbError = NbError String deriving (Show)
