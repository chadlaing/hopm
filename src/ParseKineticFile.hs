{-# LANGUAGE OverloadedStrings #-}

module ParseKineticFile
(splitHeaderData
) where

import           Data.Eq
import           Data.Ord
import qualified Data.Text.Lazy as T
import           Prelude        (Enum, Float, undefined)
import           Text.Read
import           Text.Show
import qualified Data.HashMap.Strict as HM


-- | Define all possible 96 wells for the omnilog plates
-- createExperiment is used to define the proper annotations.
data Well = A01{annotation :: T.Text
               ,value :: Float}
          | A02{annotation :: T.Text
               ,value :: Float}
          | A03{annotation :: T.Text
               ,value :: Float}
          | A04{annotation :: T.Text
               ,value :: Float}
          | A05{annotation :: T.Text
               ,value :: Float}
          | A06{annotation :: T.Text
               ,value :: Float}
          | A07{annotation :: T.Text
               ,value :: Float}
          | A08{annotation :: T.Text
               ,value :: Float}
          | A09{annotation :: T.Text
               ,value :: Float}
          | A10{annotation :: T.Text
               ,value :: Float}
          | A11{annotation :: T.Text
               ,value :: Float}
          | A12{annotation :: T.Text
               ,value :: Float}
          | B01{annotation :: T.Text
               ,value :: Float}
          | B02{annotation :: T.Text
               ,value :: Float}
          | B03{annotation :: T.Text
               ,value :: Float}
          | B04{annotation :: T.Text
               ,value :: Float}
          | B05{annotation :: T.Text
               ,value :: Float}
          | B06{annotation :: T.Text
               ,value :: Float}
          | B07{annotation :: T.Text
               ,value :: Float}
          | B08{annotation :: T.Text
               ,value :: Float}
          | B09{annotation :: T.Text
               ,value :: Float}
          | B10{annotation :: T.Text
               ,value :: Float}
          | B11{annotation :: T.Text
               ,value :: Float}
          | B12{annotation :: T.Text
               ,value :: Float}
          | C01{annotation :: T.Text
               ,value :: Float}
          | C02{annotation :: T.Text
               ,value :: Float}
          | C03{annotation :: T.Text
               ,value :: Float}
          | C04{annotation :: T.Text
               ,value :: Float}
          | C05{annotation :: T.Text
               ,value :: Float}
          | C06{annotation :: T.Text
               ,value :: Float}
          | C07{annotation :: T.Text
               ,value :: Float}
          | C08{annotation :: T.Text
               ,value :: Float}
          | C09{annotation :: T.Text
               ,value :: Float}
          | C10{annotation :: T.Text
               ,value :: Float}
          | C11{annotation :: T.Text
               ,value :: Float}
          | C12{annotation :: T.Text
               ,value :: Float}
          | D01{annotation :: T.Text
               ,value :: Float}
          | D02{annotation :: T.Text
               ,value :: Float}
          | D03{annotation :: T.Text
               ,value :: Float}
          | D04{annotation :: T.Text
               ,value :: Float}
          | D05{annotation :: T.Text
               ,value :: Float}
          | D06{annotation :: T.Text
               ,value :: Float}
          | D07{annotation :: T.Text
               ,value :: Float}
          | D08{annotation :: T.Text
               ,value :: Float}
          | D09{annotation :: T.Text
               ,value :: Float}
          | D10{annotation :: T.Text
               ,value :: Float}
          | D11{annotation :: T.Text
               ,value :: Float}
          | D12{annotation :: T.Text
               ,value :: Float}
          | E01{annotation :: T.Text
               ,value :: Float}
          | E02{annotation :: T.Text
               ,value :: Float}
          | E03{annotation :: T.Text
               ,value :: Float}
          | E04{annotation :: T.Text
               ,value :: Float}
          | E05{annotation :: T.Text
               ,value :: Float}
          | E06{annotation :: T.Text
               ,value :: Float}
          | E07{annotation :: T.Text
               ,value :: Float}
          | E08{annotation :: T.Text
               ,value :: Float}
          | E09{annotation :: T.Text
               ,value :: Float}
          | E10{annotation :: T.Text
               ,value :: Float}
          | E11{annotation :: T.Text
               ,value :: Float}
          | E12{annotation :: T.Text
               ,value :: Float}
          | F01{annotation :: T.Text
               ,value :: Float}
          | F02{annotation :: T.Text
               ,value :: Float}
          | F03{annotation :: T.Text
               ,value :: Float}
          | F04{annotation :: T.Text
               ,value :: Float}
          | F05{annotation :: T.Text
               ,value :: Float}
          | F06{annotation :: T.Text
               ,value :: Float}
          | F07{annotation :: T.Text
               ,value :: Float}
          | F08{annotation :: T.Text
               ,value :: Float}
          | F09{annotation :: T.Text
               ,value :: Float}
          | F10{annotation :: T.Text
               ,value :: Float}
          | F11{annotation :: T.Text
               ,value :: Float}
          | F12{annotation :: T.Text
               ,value :: Float}
          | G01{annotation :: T.Text
               ,value :: Float}
          | G02{annotation :: T.Text
               ,value :: Float}
          | G03{annotation :: T.Text
               ,value :: Float}
          | G04{annotation :: T.Text
               ,value :: Float}
          | G05{annotation :: T.Text
               ,value :: Float}
          | G06{annotation :: T.Text
               ,value :: Float}
          | G07{annotation :: T.Text
               ,value :: Float}
          | G08{annotation :: T.Text
               ,value :: Float}
          | G09{annotation :: T.Text
               ,value :: Float}
          | G10{annotation :: T.Text
               ,value :: Float}
          | G11{annotation :: T.Text
               ,value :: Float}
          | G12{annotation :: T.Text
               ,value :: Float}
          | H01{annotation :: T.Text
               ,value :: Float}
          | H02{annotation :: T.Text
               ,value :: Float}
          | H03{annotation :: T.Text
               ,value :: Float}
          | H04{annotation :: T.Text
               ,value :: Float}
          | H05{annotation :: T.Text
               ,value :: Float}
          | H06{annotation :: T.Text
               ,value :: Float}
          | H07{annotation :: T.Text
               ,value :: Float}
          | H08{annotation :: T.Text
               ,value :: Float}
          | H09{annotation :: T.Text
               ,value :: Float}
          | H10{annotation :: T.Text
               ,value :: Float}
          | H11{annotation :: T.Text
               ,value :: Float}
          | H12{annotation :: T.Text
               ,value :: Float}
          deriving(Ord, Eq, Show, Read)


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


-- | All possible metadata for an Experiment.
-- The required `name` and any other metadata, that will be stored
-- as a HashMap of T.Text keys and values
data Metadata = Metadata{name :: T.Text
                        ,metaFields :: HM.HashMap T.Text T.Text
} deriving(Eq, Show, Read)


-- | The Experiment type contains Plate, Well and Metadata information.
-- Created only indirectly through createExperiment
data Experiment = Experiment{plate :: Plate
                            ,wells :: HM.HashMap T.Text Well
} deriving(Eq, Show, Read)


-- | Utility function for taking the contents of a file read
-- from the command line and splitting into header and data parts.
-- The header is mined for metadata, and the data for Well values.
splitHeaderData :: T.Text -> (T.Text, T.Text)
splitHeaderData = T.breakOn "Hour"


-- | Function to be used to create an Experiment.
-- Experiment should not be used directly, as this function
-- will correctly assign the Plate and Well types and store the
-- well values.
createExperiment :: (T.Text, T.Text) -> Experiment
createExperiment (header, eData) =
    Experiment thePlate theWells
      where
        thePlate = undefined
        theWells = undefined


