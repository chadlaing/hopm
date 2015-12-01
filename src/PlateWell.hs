{-|
Module      : ParseKineticFile
Description : Analyses of omnilog kinetic data files.
Copyright   : (c) Chad Laing, 2015
License     : BSD3
Maintainer  : chadlainge@inoutbox.com
Stability   : beta
Portability : POSIX

Parses omnilog kinetic data files into appropriate data structures for statistical analyses.
-}


{-# LANGUAGE OverloadedStrings #-}

module PlateWell where

import Prelude()
import           Data.Eq
import           Data.Ord
import           Text.Read
import           Text.Show
import qualified Data.Text.Lazy as T


-- | All 20 possible plates defined for the omnilog system.
-- Create experiment is used to assign the correct description.
-- Both Plates and Wells are only to be created using createExperiment
data Plate = PM1{description :: T.Text}
           | PM2{description :: T.Text}
           | PM3{description :: T.Text}
           | PM4{description :: T.Text}
           | PM5{description :: T.Text}
           | PM6{description :: T.Text}
           | PM7{description :: T.Text}
           | PM8{description :: T.Text}
           | PM9{description :: T.Text}
           | PM10{description :: T.Text}
           | PM11{description :: T.Text}
           | PM12{description :: T.Text}
           | PM13{description :: T.Text}
           | PM14{description :: T.Text}
           | PM15{description :: T.Text}
           | PM16{description :: T.Text}
           | PM17{description :: T.Text}
           | PM18{description :: T.Text}
           | PM19{description :: T.Text}
           | PM20{description :: T.Text}
           deriving(Ord, Eq, Show, Read)