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

module ParseKineticFile
(splitHeaderData
,createExperiment
,summaryValue
,wells
,maxValue
) where

import           Prelude ((+),(-),(++), (*), Enum, Float, Double, undefined, error, Bounded, minBound, logBase, fromIntegral, round, floor )
import           Data.Eq
import           Data.Ord
import Data.Int
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Read (decimal, rational)
import Data.Either
import Data.Tuple (fst)
import           Text.Read
import           Text.Show
import Data.List (zip3, concatMap, transpose, length, filter, sum, (!!), take, maximum)
import Data.Foldable (foldl')
import Data.Function ((.), ($))
import Data.Functor (fmap)
import qualified Data.HashMap.Strict as HM
import Debug.Trace
import Control.Applicative
import Data.Bool (otherwise)
import Numeric.Integration.TanhSinh
import PlateWell


-- | All possible metadata for an Experiment.
-- The required `name` and any other metadata, that will be stored
-- as a HashMap of T.Text keys and values
newtype Metadata = Metadata {unMetadata :: HM.HashMap T.Text T.Text} deriving(Eq, Show, Read)


data WellInfo = WellInfo{annotation :: T.Text
                        ,values :: [Int]
                        ,summaryValue :: Double
                        ,maxValue :: Int
                        ,hour :: [Float]
} deriving (Eq, Show, Read)


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
        theMetadata = createMetadata header
        thePlate = createPlate $ HM.lookupDefault (error "Unknown Plate Type") "Plate Type" $ unMetadata theMetadata
        theWells = createWells thePlate eData


-- | Parse the header for plate metadata
createMetadata :: T.Text -> Metadata
createMetadata h = Metadata m
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


 -- | Return the well annotations as a HashMap according to the
 -- Plate type. We zip the wells with the annotations to ensure
 -- proper matching of the terms in createWellInfo. The actual data for the
 -- plate is sent as a single Text string to the function. It is split into
 -- lines, and each line is split on ',' with whitespace removed. This array
 -- of arrays contains row data, but we need to get columns from the data.
 -- The first row of the omnilog data should be ["Hour", "A01" ... ], so we use
 -- the transpose function to create the columns we need.
createWells :: Plate
         -> T.Text
         -> HM.HashMap Well WellInfo
createWells p eData =
    foldl' (createWellInfo hourColumn) HM.empty
                $ zip3 allWells (pmAnnotations p) dataColumns
      where
        splitLines = filter (/="") . fmap T.strip . T.split (==',') <$> T.lines eData
        (hourColumn:dataColumns) = transpose splitLines


-- | Given a list of Hours and Data columns, all with appropriate headers,
-- as well as the Well and annotation, return a properly formatted WellInfo.
-- Data.Text read does not seem to support conversion using OverloadedStrings
-- so it is manually unpacked.
createWellInfo :: [T.Text]
               -> HM.HashMap Well WellInfo
               -> (Well, T.Text, [T.Text])
               -> HM.HashMap Well WellInfo
createWellInfo (_:hs) hm (w, anno, _:ds) =
    HM.insert w WellInfo {annotation = anno
                         ,values = normalizedValues
                         ,hour = hoursAsFloat
                         ,maxValue = maximum normalizedValues
                         ,summaryValue = createSummaryValue normalizedValues} hm
  where
    valuesAsInt = fmap createIntFromText ds
    normalizedValues = let (x:_) = valuesAsInt in
                        fmap (subtractInitialValue x) valuesAsInt
    hoursAsFloat = fmap createFloatFromText hs


-- | Using decimal from Data.Text.Read, which returns an Either, with Left
-- being an Error and Right being a tuple of (number, non-number).
-- Efficient conversion from T.Text to Int and Float
-- Link http://stackoverflow.com/questions/14296989/converting-data-text-to-int-in-haskell
createIntFromText :: T.Text -> Int
createIntFromText t = case decimal t of
    Right v -> fst v
    Left e -> error e


createFloatFromText :: T.Text -> Float
createFloatFromText t = case rational t of
    Right v -> fst v
    Left e -> error "Incorrect number of hour elements in plate"


-- | Integrate the area under the curve of the kinetic data.
-- The integrate function is for continuous values from a -> b.
-- We have only a list of discrete values, evenly distributed from a -> b.
-- Thus, f(x) = x, but only for integers in the range a -> b.
-- When x = 0, it corresponds to the first value in the list (which is Time 0)
-- from the omnilog data. When x = 1 it is Time 0.25, x = 2 is Time 0.50.
-- We provide only the list of values and a range to integrate from (a -> b).
-- We therefore provide a function to give the y value for any x in the
-- continuous range. This function takes the (floor x) and (x + 1) values
-- bounding the given x, and calculates the slope. From this it calculates
-- the y value for any x. Eg. values = [0,10,21] x = 1.1, floor x = 1, x +1 = 2,
-- slope = 21 - 10 = 11, y = 10 + (1.1 - 1)*11 = 11.1
createSummaryValue :: [Int] -> Double
createSummaryValue xs = integratedValue
  where
    valuesAsDouble = fmap ((\x -> fromIntegral x ::Double) . (+1)) xs
    logValues = take 120 $ fmap (logBase 2) valuesAsDouble
    integratedValue = result $ absolute 1e-6 $ simpson nextWellValue 0
                        $ fromIntegral $ length logValues -1
      where
        nextWellValue :: Double -> Double
        nextWellValue x = finalValue
          where
            listx = floor x :: Int
            listx1 = listx + 1
            y1 = logValues !! listx
            y2 = logValues !! listx1
            slope =  y2 - y1
            finalValue = y1 + (slope * (x- fromIntegral listx))




-- | To normalize the well data, we need to subtract the initial well value
-- from all subsequent value. We also need to account for the occurrence of
-- a negative value and set it to 0.
subtractInitialValue :: Int -> Int -> Int
subtractInitialValue init x
    | subtractedValue >= 0 = subtractedValue
    | otherwise = 0
  where
    subtractedValue = x - init


-- | Generate a list of all possible wells for Well
-- This information will be combined with the appropriate label
-- given the plate type in createExperiment.
-- Because Type Well derives Enum and Bounded, we can use the
-- minBound and the infinite list to automatically return the
-- odered list of all Data Constructors for Well. In our case
-- this is A01 .. H12.
allWells :: [Well]
allWells = [(minBound :: Well) ..]
