{-# LANGUAGE ConstraintKinds #-}

module Analyze.Common where

import Data.Hashable (Hashable)
import Data.Typeable (Typeable)

type Data k = (Eq k, Hashable k, Show k, Typeable k)
