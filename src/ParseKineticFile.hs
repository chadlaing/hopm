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
,createListOfExperiment
,summaryValue
,wells
,maxValue
,groupExperimentBy
,otype
) where

import Prelude ((+),(-),(++), (*), Enum, Float, Double, undefined, error, Bounded, minBound, logBase, fromIntegral, round, floor )
import           Data.Eq
import           Data.Ord
import Data.Int
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Read (decimal, rational)
import Data.Either
import Data.Tuple (fst)
import Data.Maybe
import           Text.Read
import           Text.Show
import Data.List (zip3, concatMap, transpose, length, filter, sum, (!!), take, maximum)
import Data.Foldable (foldl')
import Data.Function ((.), ($))
import Data.Functor (fmap)
import qualified Data.HashMap.Strict as HM
import Debug.Trace
import Data.Bool (otherwise)
import Numeric.Integration.TanhSinh
import Control.Applicative
import PlateWell

-- | We are using a Meta Type to hold all of the Metadata for each experiment.
-- The data itself is storec in a Metadata record, with each field being of type
-- Meta. The unMeta instance for Meta returns a T.Text of every field.
data Meta = Name T.Text
          | OType (Maybe T.Text)
          | HType (Maybe T.Text)
          | IsolationHost (Maybe T.Text)
          | IsolationDate (Maybe T.Text)
          | PM (Maybe Plate)
          | Other [T.Text]
          deriving(Eq, Show, Read)

-- | Define the UnMeta class
class UnMeta a where
    unMeta :: a -> T.Text

-- | The only instance is for Meta, where every data constructor is provided
-- an unMeta function.
instance UnMeta Meta where
    unMeta (Name a) = a
    unMeta (OType a) = fromMaybe "" a
    unMeta (HType a) = fromMaybe "" a
    unMeta (IsolationHost a) = fromMaybe "" a
    unMeta (IsolationDate a ) = fromMaybe "" a
    unMeta (PM (Just a)) = description a
    unMeta (PM Nothing) = ""
    unMeta (Other [a]) = T.concat [a]


-- | Defining a Data Record to store all the metadata for each experiment.
data Metadata =
    Metadata {name :: Meta
             , otype :: Meta
             , htype :: Meta
             , host :: Meta
             , date :: Meta
             , pm :: Meta
             , other :: Meta
    } deriving(Eq, Show, Read)


-- | Self explanatory
defaultMetadata :: Metadata
defaultMetadata = Metadata (Name "") (OType Nothing) (HType Nothing)
                    (IsolationHost Nothing) (IsolationDate Nothing) (PM Nothing)
                    (Other [])


-- | The metadata for each well
data WellInfo = WellInfo{annotation :: T.Text
                        ,values :: [Int]
                        ,summaryValue :: Double
                        ,maxValue :: Int
                        ,hour :: [Float]
} deriving (Eq, Show, Read)


-- | The Experiment type contains Plate, Well and Metadata information.
-- Created only indirectly through createExperiment
data Experiment = Experiment{wells :: HM.HashMap Well WellInfo
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
    Experiment theWells theMetadata
      where
        theMetadata = createMetadata header
        theWells = case pm theMetadata of
                    PM (Just v) -> createWells v eData
                    PM Nothing -> error "Unknowmn PM plate"


-- | This is the function exported from the module.
-- Uses createExperiment to map over all files.
createListOfExperiment :: [T.Text] -> [Experiment]
createListOfExperiment = fmap (createExperiment . T.breakOn "Hour")


-- | Create collections of Experiments grouped by any of the Metadata
-- fields. Take in the Meta type, followed by the list of experiments to
-- generate a hashmap of every instance of that type, and the [Experiment].
-- For example `groupExperimentBy OType [Experiment]` would generate
-- "O157" --> [Experiment], "O26" --> [Experiment], ...
-- Once the data is partitioned, summary statistics comparing the [Experiment]
-- for each group can be conducted.
groupExperimentBy :: (Metadata -> Meta)
                  -> [Experiment]
                  -> HM.HashMap T.Text [Experiment]
groupExperimentBy md = foldl' (addExperimentToGroup md) HM.empty


-- | Add each individual plate to the appropriate metadata category
addExperimentToGroup :: (Metadata -> Meta)
                     -> HM.HashMap T.Text [Experiment]
                     -> Experiment
                     -> HM.HashMap T.Text [Experiment]
addExperimentToGroup md hm x = HM.insert metaKey expList hm
  where
    metaKey = unMeta $ md $ meta x
    expList = x:HM.lookupDefault [] metaKey hm


-- | Parse the header for plate metadata
createMetadata :: T.Text -> Metadata
createMetadata h = m
  where
    m = foldl' parseHeaderLine defaultMetadata (T.lines h)


-- | Split each header line, add the key value pair to
-- the HashMap, and return the whole thing for Metadata.
-- Remove the delimiter and all trailing space from both
-- the keys and the values.
parseHeaderLine :: Metadata
                -> T.Text
                -> Metadata
parseHeaderLine md t = case k of
    "Strain Name" -> md {name = Name v}
    "Plate Type" -> md {pm = PM $ Just $ createPlate v}
    "O-type" -> md {otype = OType $ Just v}
    "H-type" -> md {htype = HType $ Just v}
    "Date" -> md {date = IsolationDate $ Just v}
    "Host" -> md {host = IsolationHost $ Just v}
    _ -> md {other = Other [v]}
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


-- | Create a summary value given [Int] values for the well.
-- First log transform the data, then integrate for the area under the curve,
-- this AUC will be the returned value.
createSummaryValue :: [Int] -> Double
createSummaryValue xs = integrateList logValues
  where
    valuesAsDouble = fmap ((\x -> fromIntegral x ::Double) . (+1)) xs
    logValues = take 120 $ fmap (logBase 2) valuesAsDouble


-- | Integrate the area under the curve of the kinetic data.
-- The integrate function is for continuous values from a -> b.
-- We have only a list of discrete values, evenly distributed from a -> b.
-- Thus, f(x) = x, but only for integers in the range a -> b. Thus we employ
-- the trapezoid rule to calculate y values for x between integer values.
-- When x = 0, it corresponds to the first value in the list (which is Time 0)
-- from the omnilog data. When x = 1 it is Time 0.25, x = 2 is Time 0.50.
-- We provide only the list of values and a range to integrate from (a -> b).
-- We therefore provide a function to give the y value for any x in the
-- continuous range. This function takes the (floor x) and (x + 1) values
-- bounding the given x, and calculates the slope. From this it calculates
-- the y value for any x. Eg. values = [0,10,21] x = 1.1, floor x = 1, x +1 = 2,
-- slope = 21 - 10 = 11, y = 10 + (1.1 - 1)*11 = 11.1
integrateList :: [Double] -> Double
integrateList xs = result $ absolute 1e-6 $ parSimpson nextWellValue 0
                    $ fromIntegral $ length xs -1
  where
    nextWellValue :: Double -> Double
    nextWellValue x = finalValue
      where
        listx = floor x :: Int
        listx1 = listx + 1
        y1 = xs !! listx
        y2 = xs !! listx1
        slope =  y2 - y1
        finalValue = y1 + (slope * (x - fromIntegral listx))




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

-- | To L-Histidine +pH 4.5", calues = [0,0,1,1,1,1,1,2,,4,4,4,
