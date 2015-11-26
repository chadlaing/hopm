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
{-# LANGUAGE DeriveGeneric              #-}

module ParseKineticFile
(splitHeaderData
,createExperiment
) where

import           Data.Eq
import           Data.Ord
import qualified Data.Text.Lazy as T
import           Prelude        (Enum, Float, undefined, error, Bounded, minBound)
import           Text.Read
import           Text.Show
import Data.Hashable
import GHC.Generics               (Generic)
import Data.List (zip)
import Data.Foldable (foldl')
import Data.Function ((.), ($))
import qualified Data.HashMap.Strict as HM


-- | Define all possible 96 wells for the omnilog plates
-- createExperiment is used to define the proper annotations.
-- In order for type Well to be used as a hash key, it needs to be
-- an instance of Hashable (requires Data.Hashable), and to derive the
-- type Generic (requires GHC.Generics, and enabling the DeriveGeneric
-- pragma).
data Well = A01 | A02 | A03 | A04 | A05 | A06 | A07 | A08 | A09 | A10 | A11 | A12
          | B01 | B02 | B03 | B04 | B05 | B06 | B07 | B08 | B09 | B10 | B11 | B12
          | C01 | C02 | C03 | C04 | C05 | C06 | C07 | C08 | C09 | C10 | C11 | C12
          | D01 | D02 | D03 | D04 | D05 | D06 | D07 | D08 | D09 | D10 | D11 | D12
          | E01 | E02 | E03 | E04 | E05 | E06 | E07 | E08 | E09 | E10 | E11 | E12
          | F01 | F02 | F03 | F04 | F05 | F06 | F07 | F08 | F09 | F10 | F11 | F12
          | G01 | G02 | G03 | G04 | G05 | G06 | G07 | G08 | G09 | G10 | G11 | G12
          | H01 | H02 | H03 | H04 | H05 | H06 | H07 | H08 | H09 | H10 | H11 | H12
          deriving(Ord, Eq, Show, Read, Enum, Bounded, Generic)

instance Hashable Well

data WellInfo = WellInfo{annotation :: T.Text
                        ,value :: Float
} deriving (Eq, Show, Read)

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
newtype Metadata = Metadata {unMetadata :: HM.HashMap T.Text T.Text} deriving(Eq, Show, Read)


-- | The Experiment type contains Plate, Well and Metadata information.
-- Created only indirectly through createExperiment
data Experiment = Experiment{plate :: Plate
                            ,wells :: HM.HashMap Well WellInfo
                            ,meta :: Metadata
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
    Experiment thePlate theWells theMetadata
      where
        theMetadata = getMetadata header
        thePlate = getPlate $ HM.lookupDefault (error "Unknown Plate Type") "Plate Type" $ unMetadata theMetadata
        theWells = getWells thePlate


-- | Parse the header for plate metadata
getMetadata :: T.Text -> Metadata
getMetadata h = Metadata m
  where
    m = foldl' parseHeaderLine HM.empty (T.lines h)


-- | Split each header line, add the key value pair to
-- the HashMap, and return the whole thing for Metadata.
-- Remove the delimiter and all trailing space from both
-- the keys and the values.
parseHeaderLine :: HM.HashMap T.Text T.Text
                -> T.Text
                -> HM.HashMap T.Text T.Text
parseHeaderLine hm t = HM.insert k v hm
  where
    (kk, vv) = T.breakOn "," t
    k = T.strip kk
    v = T.strip $ T.drop 1 vv



-- | Returns an actual plate Type based on the
-- metadata text.
getPlate :: T.Text -> Plate
getPlate x = case x of
    "PM1" -> PM1 "Carbon utilization assays"
    "PM2" -> PM2 "Carbon utilization assays"
    "PM3" -> PM3 "Nitrogen utilization assays"
    "PM4" -> PM4 "Phosphorus - Sulfur utilization assays"
    "PM5" -> PM5 "Biosynthetic pathway/nutrient stimulation"
    "PM6" -> PM6 "Nitrogen utilization assays"
    "PM7" -> PM7 "Nitrogen utilization assays"
    "PM8" -> PM8 "Nitrogen utilization assays"
    "PM9" -> PM9 "Osmotic/Ionic response assays"
    "PM10" -> PM10 "pH response assays"
    "PM11" -> PM11 "Bacterial chemical sensitivity assays"
    "PM12" -> PM12 "Bacterial chemical sensitivity assays"
    "PM13" -> PM13 "Bacterial chemical sensitivity assays"
    "PM14" -> PM14 "Bacterial chemical sensitivity assays"
    "PM15" -> PM15 "Bacterial chemical sensitivity assays"
    "PM16" -> PM16 "Bacterial chemical sensitivity assays"
    "PM17" -> PM17 "Bacterial chemical sensitivity assays"
    "PM18" -> PM18 "Bacterial chemical sensitivity assays"
    "PM19" -> PM19 "Bacterial chemical sensitivity assays"
    "PM20" -> PM20 "Bacterial chemical sensitivity assays"
    _ -> error "Unknown PM plate"


 -- | Return the well annotations as a HashMap according to the
 -- Plate type. We zip the wells with the annotations to ensure
 -- proper matching of the terms in createWellInfo
getWells :: Plate -> HM.HashMap Well WellInfo
getWells p = case p of
    PM1 _ -> foldl' createWellInfo HM.empty $ zip allWells pm1Annotations


createWellInfo :: HM.HashMap Well WellInfo
               -> (Well, T.Text)
               -> HM.HashMap Well WellInfo
createWellInfo hm (w, anno) = HM.insert w WellInfo {annotation = anno, value = 0.05} hm


-- | Generate a list of all possible wells for Well
-- This information will be combined with the appropriate label
-- given the plate type in createExperiment
allWells :: [Well]
allWells = [(minBound :: Well) ..]


-- | Bulk list of PM1 well annotations
pm1Annotations :: [T.Text]
pm1Annotations = ["Negative Control","L-Arabinose","N-Acetyl-D-Glucosamine","D-Saccharic Acid","Succinic Acid","D-Galactose","L-Aspartic Acid","L-Proline","D-Alanine","D-Trehalose","D-Mannose","Dulcitol","D-Serine","D-Sorbitol","Glycerol","L-Fucose","D-Glucuronic Acid","D-Gluconic Acid","D,L-a-Glycerol-Phosphate","D-Xylose","L-Lactic Acid","Sodium Formate","D-Mannitol","L-Glutamic Acid","D-Glucose-6-Phosphate","D-Galactonic Acid-g-Lactone","D,L-Malic Acid","D-Ribose","Tween 20","L-Rhamnose","D-Fructose","Acetic Acid","D-Glucose","D-Maltose","D-Melibiose","Thymidine","L-Asparagine","D-Aspartic Acid","D-Glucosaminic Acid","1,2-Propanediol","Tween 40","a-Keto-Glutaric Acid","a-Keto-Butyric Acid","a-Methyl-D-Galactoside","a-D-Lactose","Lactulose","Sucrose","Uridine","L-Glutamine","m-Tartaric Acid","a-D-Glucose-1-Phosphate","D-Fructose-6-Phosphate","Tween 80","a-Hydroxy-Glutaric Acid-g-Lactone","a-Hydroxy-Butyric Acid","b-Methyl-D-Glucoside","Adonitol","Maltotriose","2-Deoxy-Adenosine","Adenosine","Gly-Asp","Citric Acid","myo-Inositol","D-Threonine","Fumaric Acid","Bromo-Succinic Acid","Propionic Acid","Mucic Acid","Glycolic Acid","Glyoxylic Acid","D-Cellobiose","Inosine","Gly-Glu","Tricarballylic Acid","L-Serine","L-Threonine","L-Alanine","Ala-Gly","Acetoacetic Acid","N-Acetyl-b-D-Mannosamine","Mono-Methyl Succinate","Methyl Pyruvate","D-Malic Acid","L-Malic Acid","Gly-Pro","p-Hydroxy-Phenylacetic Acid","m-Hydroxy-Phenylacetic Acid","Tyramine","D-Psicose","L-Lyxose","Glucuronamide","Pyruvic Acid","L-Galactonic Acid-g-Lactone","D-Galacturonic Acid","b-Phenylethylamine","Ethanolamine"]