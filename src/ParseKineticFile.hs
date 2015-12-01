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
,summaryValue
,wells
) where

import           Prelude ((+),(-),(++), Enum, Float, Double, undefined, error, Bounded, minBound, logBase, fromIntegral )
import           Data.Eq
import           Data.Ord
import Data.Int
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Read (decimal, rational)
import Data.Either
import Data.Tuple (fst)
import           Text.Read
import           Text.Show
import Data.List (zip3, concatMap, transpose, length, filter, sum, (!!), take)
import Data.Foldable (foldl')
import Data.Function ((.), ($))
import Data.Functor (fmap)
import qualified Data.HashMap.Strict as HM
import Debug.Trace
import Control.Applicative
import Data.Bool (otherwise)
import Data.Hashable
import GHC.Generics               (Generic)
import PlateWell

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



-- | All possible metadata for an Experiment.
-- The required `name` and any other metadata, that will be stored
-- as a HashMap of T.Text keys and values
newtype Metadata = Metadata {unMetadata :: HM.HashMap T.Text T.Text} deriving(Eq, Show, Read)


data WellInfo = WellInfo{annotation :: T.Text
                        ,values :: [Int]
                        ,summaryValue :: Double
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



-- | Returns an actual plate Type based on the
-- metadata text.
createPlate :: T.Text -> Plate
createPlate x = case x of
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
                         ,values = valuesAsInt
                         ,hour = valuesAsFloat
                         ,summaryValue = createSummaryValue valuesAsFloat valuesAsInt} hm
  where
    valuesAsInt = fmap createIntFromText ds
    valuesAsFloat = fmap createFloatFromText hs


-- | Using decimal from Data.Text.Read, which returns an Either, with Left
-- being an Error and Right being a tuple of (number, non-number).
-- Efficient conversion from T.Text to Int
-- Link http://stackoverflow.com/questions/14296989/converting-data-text-to-int-in-haskell
createIntFromText :: T.Text -> Int
createIntFromText t = case decimal t of
    Right v -> fst v
    Left e -> error e


createFloatFromText :: T.Text -> Float
createFloatFromText t = case rational t of
    Right v -> fst v
    Left e -> error "Incorrect number of hour elements in plate"


-- | Integrate the area under the curve of the kinetic data, after subtracting
-- the initial well value from all subsequent wells.
createSummaryValue :: [Float] -> [Int] -> Double
createSummaryValue hrs (x:xs) = integratedValue
  where
    normalizedValues = fmap (subtractInitialValue x) (x:xs)
    logValues = take 120 $ fmap (logBase 2 . (+1 ) . fromIntegral) normalizedValues
    integratedValue = sum logValues
--     integratedValue = result $ absolute 1e-6 $ parTrap nextWellValue 0 $ fromIntegral $ length logValues
--       where
--         nextWellValue :: Double -> Double
--         nextWellValue x = logValues !! x


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


-- | Bulk list of all PM well annotations
-- PM1 -- PM20
pmAnnotations :: Plate -> [T.Text]
pmAnnotations p = case p of
    PM1 _ -> ["Negative Control","L-Arabinose","N-Acetyl-D-Glucosamine","D-Saccharic Acid","Succinic Acid","D-Galactose","L-Aspartic Acid","L-Proline","D-Alanine","D-Trehalose","D-Mannose","Dulcitol","D-Serine","D-Sorbitol","Glycerol","L-Fucose","D-Glucuronic Acid","D-Gluconic Acid","D,L-a-Glycerol-Phosphate","D-Xylose","L-Lactic Acid","Sodium Formate","D-Mannitol","L-Glutamic Acid","D-Glucose-6-Phosphate","D-Galactonic Acid-g-Lactone","D,L-Malic Acid","D-Ribose","Tween 20","L-Rhamnose","D-Fructose","Acetic Acid","D-Glucose","D-Maltose","D-Melibiose","Thymidine","L-Asparagine","D-Aspartic Acid","D-Glucosaminic Acid","1,2-Propanediol","Tween 40","a-Keto-Glutaric Acid","a-Keto-Butyric Acid","a-Methyl-D-Galactoside","a-D-Lactose","Lactulose","Sucrose","Uridine","L-Glutamine","m-Tartaric Acid","a-D-Glucose-1-Phosphate","D-Fructose-6-Phosphate","Tween 80","a-Hydroxy-Glutaric Acid-g-Lactone","a-Hydroxy-Butyric Acid","b-Methyl-D-Glucoside","Adonitol","Maltotriose","2-Deoxy-Adenosine","Adenosine","Gly-Asp","Citric Acid","myo-Inositol","D-Threonine","Fumaric Acid","Bromo-Succinic Acid","Propionic Acid","Mucic Acid","Glycolic Acid","Glyoxylic Acid","D-Cellobiose","Inosine","Gly-Glu","Tricarballylic Acid","L-Serine","L-Threonine","L-Alanine","Ala-Gly","Acetoacetic Acid","N-Acetyl-b-D-Mannosamine","Mono-Methyl Succinate","Methyl Pyruvate","D-Malic Acid","L-Malic Acid","Gly-Pro","p-Hydroxy-Phenylacetic Acid","m-Hydroxy-Phenylacetic Acid","Tyramine","D-Psicose","L-Lyxose","Glucuronamide","Pyruvic Acid","L-Galactonic Acid-g-Lactone","D-Galacturonic Acid","b-Phenylethylamine","Ethanolamine"]
    PM2 _ -> ["carbon utilization assays","Negative Control","Chondroitin Sulfate C","α-Cyclodextrin","β-Cyclodextrin","γ-Cyclodextrin","Dextrin","Gelatin","Glycogen","Inulin","Laminarin","Mannan","Pectin","N-Acetyl-D-Galactosamine","N-Acetyl-Neuraminic Acid","β-D-Allose","Amygdalin","D-Arabinose","D-Arabitol","L-Arabitol","Arbutin","2-Deoxy-D-Ribose","m-Erythritol","D-Fucose","3-O-β-D-Galactopyranosyl-D-Arabinose","β-Gentiobiose","L-Glucose","D-Lactitol","D-Melezitose","Maltitol","α-Methyl-D-Glucoside","β-Methyl-D-Galactoside","3-O-Methyl-D-Glucose","β-Methyl-D-Glucuronic Acid","α-Methyl-D-Mannoside","β-Methyl-D-Xylopyranoside","Palatinose","D-Raffinose","D-Salicin","Sedoheptulosan","L-Sorbose","Stachyose","D-Tagatose","Turanose","Xylitol","N-Acetyl-D-Glucosaminitol","γ-Amino-n-Butyric Acid","δ-Amino-Valeric Acid","Butyric Acid","Capric Acid","Caproic Acid","Citraconic Acid","D-Citramalic Acid","D-Glucosamine","2-Hydroxy-Benzoic Acid","4-Hydroxy-Benzoic Acid","β-Hydroxy-Butyric Acid","γ-Hydroxy-Butyric Acid","α-Keto-Valeric Acid","Itaconic Acid","5-Keto-D-Gluconic Acid","D-Lactic Acid Methyl Ester","Malonic Acid","Melibionic Acid","Oxalic Acid","Oxalomalic Acid","Quinic Acid","D-Ribono-1,4-Lactone","Sebacic Acid","Sorbic Acid","Succinamic Acid","D-Tartaric Acid","L-Tartaric Acid","Acetamide","L-Alaninamide","N-Acetyl-L-Glutamic Acid","L-Arginine","Glycine","L-Histidine","L-Homoserine","4-Hydroxy-L-Proline [trans]","L-Isoleucine","L-Leucine","L-Lysine","L-Methionine","L-Ornithine","L-Phenylalanine","L-Pyroglutamic Acid","L-Valine","D,L-Carnitine","Butylamine [sec]","D,L-Octopamine","Putrescine","Dihydroxy-Acetone","2,3-Butanediol","2,3-Butanedione","3-Hydroxy-2-Butanone"]
    PM3 _ -> ["Negative Control","Ammonia","Sodium Nitrite","Sodium Nitrate","Urea","Biuret","L-Alanine","L-Arginine","L-Asparagine","L-Aspartic Acid","L-Cysteine","L-Glutamic Acid","L-Glutamine","Glycine","L-Histidine","L-Isoleucine","L-Leucine","L-Lysine","L-Methionine","L-Phenylalanine","L-Proline","L-Serine","L-Threonine","L-Tryptophan","L-Tyrosine","L-Valine","D-Alanine","D-Asparagine","D-Aspartic Acid","D-Glutamic Acid","D-Lysine","D-Serine","D-Valine","L-Citrulline","L-Homoserine","L-Ornithine","N-Acetyl-L-Glutamic Acid","N-Phthaloyl-L-Glutamic Acid","L-Pyroglutamic Acid","Hydroxylamine","Methylamine","N-Amylamine","N-Butylamine","Ethylamine","Ethanolamine","Ethylenediamine","Putrescine","Agmatine","Histamine","β-Phenylethylamine","Tyramine","Acetamide","Formamide","Glucuronamide","D,L-Lactamide","D-Glucosamine","D-Galactosamine","D-Mannosamine","N-Acetyl-D-Glucosamine","N-Acetyl-D-Galactosamine","N-Acetyl-β-D-Mannosamine","Adenine","Adenosine","Cytidine","Cytosine","Guanine","Guanosine","Thymine","Thymidine","Uracil","Uridine","Inosine","Xanthine","Xanthosine","Uric Acid","Alloxan","Allantoin","Parabanic Acid","D,L-α-Amino-Butyric Acid","γ-Amino-n-Butyric Acid","ε-Amino-N-Caproic Acid","D,L-α-Amino-Caprylic Acid","δ-Amino-Valeric Acid","L-Norvaline","Ala-Asp","Ala-Gln","Ala-Glu","Ala-Gly","Ala-His","Ala-Leu","Ala-Thr","Gly-Asn","Gly-Gln","Gly-Glu","Gly-Met","Met-Ala"]
    PM4 _ -> ["Negative Control #1","Sodium Phosphate","Sodium Pyrophosphate","Trimetaphosphate","Tripolyphosphate","Triethyl Phosphate","Hypophosphite","Adenosine-2′-Monophosphate","Adenosine-3′-Monophosphate","Adenosine-5′-Monophosphate","Adenosine-2′,3′-Cyclic Monophosphate","Adenosine-3′,5′-Cyclic Monophosphate","Thiophosphate #1","Dithiophosphate #1","D,L-α-Glycerol-Phosphate","β-Glycerol Phosphate","Carbamyl Phosphate","D-2-Phospho-Glyceric Acid","D-3-Phospho-Glyceric Acid","Guanosine-2′-Monophosphate","Guanosine-3′-Monophosphate","Guanosine-5′-Monophosphate","Guanosine-2′,3′-Cyclic Monophosphate","Guanosine-3′,5′-Cyclic Monophosphate","Phosphoenol Pyruvate","Phospho-Glycolic Acid","α-D-Glucose-1-Phosphate","D-Glucose-6-Phosphate","2-Deoxy-D-Glucose-6-Phosphate","D-Glucosamine-6-Phosphate","6-Phospho-Gluconic Acid","Cytidine-2′-Monophosphate","Cytidine-3′-Monophosphate","Cytidine-5′-Monophosphate","Cytidine-2′,3′-Cyclic Monophosphate","Cytidine-3′,5′-Cyclic Monophosphate","D-Mannose-1-Phosphate","D-Mannose-6-Phosphate","Cysteamine-S-Phosphate","Phospho-L-Arginine","O-Phospho-D-Serine","O-Phospho-L-Serine","O-Phospho-L-Threonine","Uridine-2′-Monophosphate","Uridine-3′-Monophosphate","Uridine-5′-Monophosphate","Uridine-2′,3′-Cyclic Monophosphate","Uridine-3′,5′-Cyclic Monophosphate","O-Phospho-D-Tyrosine","O-Phospho-L-Tyrosine","Phosphocreatine","Phosphorylcholine","O-Phosphoryl-Ethanolamine","Phosphono Acetic Acid","2-Aminoethyl Phosphonic Acid","Methylene Diphosphonic Acid","Thymidine-3′-Monophosphate","Thymidine-5′-Monophosphate","Inositol Hexaphosphate","Thymidine 3′,5′-Cyclic Monophosphate","Negative Control #2","Sulfate","Sodium Thiosulfate","Tetrathionate","Thiophosphate #2","Dithiophosphate #2","L-Cysteine","D-Cysteine","Cys-Gly","L-Cysteic Acid","Cysteamine","L-Cysteine Sulfinic Acid","N-Acetyl-L-Cysteine","S-Methyl-L-Cysteine","Cystathionine","Lanthionine","Glutathione","D,L-Ethionine","L-Methionine","D-Methionine","Gly-Met","N-Acetyl-D,L-Methionine","L-Methionine Sulfoxide","L-Methionine Sulfone","L-Djenkolic Acid","Thiourea","1-Thio-β-D-Glucose","D,L-Lipoamide","Taurocholic Acid","Taurine","Hypotaurine","p-Amino Benzene Sulfonic Acid","Butane Sulfonic Acid","2-Hydroxyethane Sulfonic Acid","Methane Sulfonic Acid","Tetramethylene Sulfone"]
    PM5 _ -> ["Negative Control","Positive Control","L-Alanine","L-Arginine","L-Asparagine","L-Aspartic Acid","L-Cysteine","L-Glutamic Acid","Adenosine-3′,5′-Cyclic Monophosphate","Adenine","Adenosine","2′-Deoxy-Adenosine","L-Glutamine","Glycine","L-Histidine","L-Isoleucine","L-Leucine","L-Lysine","L-Methionine","L-Phenylalanine","Guanosine-3′,5′-Cyclic Monophosphate","Guanine","Guanosine","2′-Deoxy-Guanosine","L-Proline","L-Serine","L-Threonine","L-Tryptophan","L-Tyrosine","L-Valine","L-Isoleucine + L-Valine","4-Hydroxy-L-Proline [trans]","[5]4-Amino-Imidazole-4[5]-Carboxamide","Hypoxanthine","Inosine","2′-Deoxy-Inosine","L-Ornithine","L-Citrulline","Chorismic Acid","Shikimic Acid [-]","L-Homoserine Lactone","D-Alanine","D-Aspartic Acid","D-Glutamic Acid","D,L-Diamino-Pimelic Acid","Cytosine","Cytidine","2′-Deoxy-Cytidine","Putrescine","Spermidine","Spermine","Pyridoxine","Pyridoxal","Pyridoxamine","β-Alanine","D-Pantothenic Acid","Orotic Acid","Uracil","Uridine","2′-Deoxy-Uridine","Quinolinic Acid","Nicotinic Acid","Nicotinamide","β-Nicotinamide Adenine Dinucleotide","δ-Amino-Levulinic Acid","Hematin","Deferoxamine Mesylate","D-Glucose","N-Acetyl-D-Glucosamine","Thymine","Glutathione","Thymidine","Oxaloacetic Acid","D-Biotin","Cyanocobalamin","p-Amino-Benzoic Acid","Folic Acid","Inosine + Thiamine","Thiamine","Thiamine Pyrophosphate","Riboflavin","Pyrrolo-Quinoline Quinone","Menadione","myo-Inositol","Butyric Acid","α-Hydroxy-Butyric Acid","α-Keto-Butyric Acid","Sodium Caprylate","D,L-α-Lipoic Acid","D,L-Mevalonic Acid Lactone","D,L-Carnitine","Choline","Tween 20","Tween 40","Tween 60","Tween 80"]
    PM6 _ -> ["Negative Control","L-Glutamine","Ala-Ala","Ala-Arg","Ala-Asn","Ala-Glu","Ala-Gly","Ala-His","Ala-Leu","Ala-Lys","Ala-Phe","Ala-Pro","Ala-Ser","Ala-Thr","Ala-Trp","Ala-Tyr","Arg-Ala","Arg-Arg","Arg-Asp","Arg-Gln","Arg-Glu","Arg-Ile","Arg-Leu","Arg-Lys","Arg-Met","Arg-Phe","Arg-Ser","Arg-Trp","Arg-Tyr","Arg-Val","Asn-Glu","Asn-Val","Asp-Asp","Asp-Glu","Asp-Leu","Asp-Lys","Asp-Phe","Asp-Trp","Asp-Val","Cys-Gly","Gln-Gln","Gln-Gly","Glu-Asp","Glu-Glu","Glu-Gly","Glu-Ser","Glu-Trp","Glu-Tyr","Glu-Val","Gly-Ala","Gly-Arg","Gly-Cys","Gly-Gly","Gly-His","Gly-Leu","Gly-Lys","Gly-Met","Gly-Phe","Gly-Pro","Gly-Ser","Gly-Thr","Gly-Trp","Gly-Tyr","Gly-Val","His-Asp","His-Gly","His-Leu","His-Lys","His-Met","His-Pro","His-Ser","His-Trp","His-Tyr","His-Val","Ile-Ala","Ile-Arg","Ile-Gln","Ile-Gly","Ile-His","Ile-Ile","Ile-Met","Ile-Phe","Ile-Pro","Ile-Ser","Ile-Trp","Ile-Tyr","Ile-Val","Leu-Ala","Leu-Arg","Leu-Asp","Leu-Glu","Leu-Gly","Leu-Ile","Leu-Leu","Leu-Met","Leu-Phe"]
    PM7 _ -> ["Negative Control","L-Glutamine","Leu-Ser","Leu-Trp","Leu-Val","Lys-Ala","Lys-Arg","Lys-Glu","Lys-Ile","Lys-Leu","Lys-Lys","Lys-Phe","Lys-Pro","Lys-Ser","Lys-Thr","Lys-Trp","Lys-Tyr","Lys-Val","Met-Arg","Met-Asp","Met-Gln","Met-Glu","Met-Gly","Met-His","Met-Ile","Met-Leu","Met-Lys","Met-Met","Met-Phe","Met-Pro","Met-Trp","Met-Val","Phe-Ala","Phe-Gly","Phe-Ile","Phe-Phe","Phe-Pro","Phe-Ser","Phe-Trp","Pro-Ala","Pro-Asp","Pro-Gln","Pro-Gly","Pro-Hyp","Pro-Leu","Pro-Phe","Pro-Pro","Pro-Tyr","Ser-Ala","Ser-Gly","Ser-His","Ser-Leu","Ser-Met","Ser-Phe","Ser-Pro","Ser-Ser","Ser-Tyr","Ser-Val","Thr-Ala","Thr-Arg","Thr-Glu","Thr-Gly","Thr-Leu","Thr-Met","Thr-Pro","Trp-Ala","Trp-Arg","Trp-Asp","Trp-Glu","Trp-Gly","Trp-Leu","Trp-Lys","Trp-Phe","Trp-Ser","Trp-Trp","Trp-Tyr","Tyr-Ala","Tyr-Gln","Tyr-Glu","Tyr-Gly","Tyr-His","Tyr-Leu","Tyr-Lys","Tyr-Phe","Tyr-Trp","Tyr-Tyr","Val-Arg","Val-Asn","Val-Asp","Val-Gly","Val-His","Val-Ile","Val-Leu","Val-Tyr","Val-Val","γ-Glu-Gly"]
    PM8 _ -> ["Negative Control","L-Glutamine","Ala-Asp","Ala-Gln","Ala-Ile","Ala-Met","Ala-Val","Asp-Ala","Asp-Gln","Asp-Gly","Glu-Ala","Gly-Asn","Gly-Asp","Gly-Ile","His-Ala","His-Glu","His-His","Ile-Asn","Ile-Leu","Leu-Asn","Leu-His","Leu-Pro","Leu-Tyr","Lys-Asp","Lys-Gly","Lys-Met","Met-Thr","Met-Tyr","Phe-Asp","Phe-Glu","Gln-Glu","Phe-Met","Phe-Tyr","Phe-Val","Pro-Arg","Pro-Asn","Pro-Glu","Pro-Ile","Pro-Lys","Pro-Ser","Pro-Trp","Pro-Val","Ser-Asn","Ser-Asp","Ser-Gln","Ser-Glu","Thr-Asp","Thr-Gln","Thr-Phe","Thr-Ser","Trp-Val","Tyr-Ile","Tyr-Val","Val-Ala","Val-Gln","Val-Glu","Val-Lys","Val-Met","Val-Phe","Val-Pro","Val-Ser","β-Ala-Ala","β-Ala-Gly","β-Ala-His","Met-β-Ala","β-Ala-Phe","D-Ala-D-Ala","D-Ala-Gly","D-Ala-Leu","D-Leu-D-Leu","D-Leu-Gly","D-Leu-Tyr","γ-Glu-Gly","γ-D-Glu-Gly","Gly-D-Ala","Gly-D-Asp","Gly-D-Ser","Gly-D-Thr","Gly-D-Val","Leu-β-Ala","Leu-D-Leu","Phe-β-Ala","Ala-Ala-Ala","D-Ala-Gly-Gly","Gly-Gly-Ala","Gly-Gly-D-Leu","Gly-Gly-Gly","Gly-Gly-Ile","Gly-Gly-Leu","Gly-Gly-Phe","Val-Tyr-Val","Gly-Phe-Phe","Leu-Gly-Gly","Leu-Leu-Leu","Phe-Gly-Gly","Tyr-Gly-Gly"]
    PM9 _ -> ["1% NaCl","2% NaCl","3% NaCl","4% NaCl","5% NaCl","5.5% NaCl","6% NaCl","6.5% NaCl","7% NaCl","8% NaCl","9% NaCl","10% NaCl","6% NaCl","Betaine + 6% NaCl","N,N-Dimethyl Glycine + 6% NaCl","Sarcosine + 6% NaCl","Dimethyl Sulphonyl Propionate + 6% NaCl","MOPS + 6% NaCl","Ectoine + 6% NaCl","Choline + 6% NaCl","Phosphorylcholine + 6% NaCl","Creatine + 6% NaCl","Creatinine + 6% NaCl","L-Carnitine + 6% NaCl","Potassium Chloride + 6% NaCl","L-Proline + 6% NaCl","N-Acetyl-L-Glutamine + 6% NaCl","β-Glutamic Acid + 6% NaCl","γ-Amino-n-Butyric Acid + 6% NaCl","Glutathione + 6% NaCl","Glycerol + 6% NaCl","D-Trehalose + 6% NaCl","Trimethylamine-N-Oxide + 6% NaCl","Trimethylamine + 6% NaCl","Octopine + 6% NaCl","Trigonelline + 6% NaCl","3% Potassium Chloride","4% Potassium Chloride","5% Potassium Chloride","6% Potassium Chloride","2% Sodium Sulfate","3% Sodium Sulfate","4% Sodium Sulfate","5% Sodium Sulfate","5% Ethylene Glycol","10% Ethylene Glycol","15% Ethylene Glycol","20% Ethylene Glycol","1% Sodium Formate","2% Sodium Formate","3% Sodium Formate","4% Sodium Formate","5% Sodium Formate","6% Sodium Formate","2% Urea","3% Urea","4% Urea","5% Urea","6% Urea","7% Urea","1% Sodium Lactate","2% Sodium Lactate","3% Sodium Lactate","4% Sodium Lactate","5% Sodium Lactate","6% Sodium Lactate","7% Sodium Lactate","8% Sodium Lactate","9% Sodium Lactate","10% Sodium Lactate","11% Sodium Lactate","12% Sodium Lactate","20mM Sodium Phosphate + pH 7","50mM Sodium Phosphate + pH 7","100mM Sodium Phosphate + pH 7","200mM Sodium Phosphate + pH 7","20mM Sodium Benzoate + pH 5.2","50mM Sodium Benzoate + pH 5.2","100mM Sodium Benzoate + pH 5.2","200mM Sodium Benzoate + pH 5.2","10mM Ammonium Sulfate + pH 8","20mM Ammonium Sulfate + pH 8","50mM Ammonium Sulfate + pH 8","100mM Ammonium Sulfate + pH 8","10mM Sodium Nitrate","20mM Sodium Nitrate","40mM Sodium Nitrate","60mM Sodium Nitrate","80mM Sodium Nitrate","100mM Sodium Nitrate","10mM Sodium Nitrite","20mM Sodium Nitrite","40mM Sodium Nitrite","60mM Sodium Nitrite","80mM Sodium Nitrite","100mM Sodium Nitrite"]
    PM10 _ -> ["pH 3.5","pH 4","pH 4.5","pH 5","pH 5.5","pH 6","pH 7","pH 8","pH 8.5","pH 9","pH 9.5","pH 10","pH 4.5","L-Alanine + pH 4.5","L-Arginine + pH 4.5","L-Asparagine + pH 4.5","L-Aspartic Acid + pH 4.5","L-Glutamic Acid + pH 4.5","L-Glutamine + pH 4.5","Glycine + pH 4.5","L-Histidine + pH 4.5","L-Isoleucine + pH 4.5","L-Leucine + pH 4.5","L-Lysine + pH 4.5","L-Methionine + pH 4.5","L-Phenylalanine + pH 4.5","L-Proline + pH 4.5","L-Serine + pH 4.5","L-Threonine + pH 4.5","L-Tryptophan + pH 4.5","L-Citrulline + pH 4.5","L-Valine + pH 4.5","4-Hydroxy-L-Proline [trans] + pH 4.5","L-Ornithine + pH 4.5","L-Homoarginine + pH 4.5","L-Homoserine + pH 4.5","Anthranilic Acid + pH 4.5","L-Norleucine + pH 4.5","L-Norvaline + pH 4.5","D,L-α-Amino-Butyric Acid + pH 4.5","p-Amino-Benzoic Acid + pH 4.5","L-Cysteic Acid + pH 4.5","D-Lysine + pH 4.5","5-Hydroxy-L-Lysine + pH 4.5","5-Hydroxy-L-Tryptophan + pH 4.5","D,L-Diamino-Pimelic Acid + pH 4.5","Trimethylamine-N-Oxide + pH 4.5","Urea + pH 4.5","pH 9.5","L-Alanine + pH 9.5","L-Arginine + pH 9.5","L-Asparagine + pH 9.5","L-Aspartic Acid + pH 9.5","L-Glutamic Acid + pH 9.5","L-Glutamine + pH 9.5","Glycine + pH 9.5","L-Histidine + pH 9.5","L-Isoleucine + pH 9.5","L-Leucine + pH 9.5","L-Lysine + pH 9.5","L-Methionine + pH 9.5","L-Phenylalanine + pH 9.5","L-Proline + pH 9.5","L-Serine + pH 9.5","L-Threonine + pH 9.5","L-Tryptophan + pH 9.5","L-Tyrosine + pH 9.5","L-Valine + pH 9.5","4-Hydroxy-L-Proline [trans] + pH 9.5","L-Ornithine + pH 9.5","L-Homoarginine + pH 9.5","L-Homoserine + pH 9.5","Anthranilic Acid + pH 9.5","L-Norleucine + pH 9.5","L-Norvaline + pH 9.5","Agmatine + pH 9.5","Cadaverine + pH 9.5","Putrescine + pH 9.5","Histamine + pH 9.5","β-Phenylethylamine + pH 9.5","Tyramine + pH 9.5","Creatine + pH 9.5","Trimethylamine-N-Oxide + pH 9.5","Urea + pH 9.5","5-Bromo-4-chloro-3-indolyl Caprylate","5-Bromo-4-chloro-3-indolyl α-D-Glucoside","5-Bromo-4-chloro-3-indolyl β-D-Glucoside","5-Bromo-4-chloro-3-indolyl α-D-Galactoside","5-Bromo-4-chloro-3-indolyl β-D-Galactoside","5-Bromo-4-chloro-3-indolyl α-D-Glucuronide","5-Bromo-4-chloro-3-indolyl β-D-Glucuronide","5-Bromo-4-chloro-3-indolyl β-D-Glucosaminide","5-Bromo-4-chloro-3-indolyl β-D-Galactosaminide","5-Bromo-4-chloro-3-indolyl α-D-Mannoside","5-Bromo-4-chloro-3-indolyl Phosphate","5-Bromo-4-chloro-3-indolyl Sulfate"]
    PM11 _ -> ["Amikacin #1","Amikacin #2","Amikacin #3","Amikacin #4","Chlortetracycline #1","Chlortetracycline #2","Chlortetracycline #3","Chlortetracycline #4","Lincomycin #1","Lincomycin #2","Lincomycin #3","Lincomycin #4","Amoxicillin #1","Amoxicillin #2","Amoxicillin #3","Amoxicillin #4","Cloxacillin #1","Cloxacillin #2","Cloxacillin #3","Cloxacillin #4","Lomefloxacin #1","Lomefloxacin #2","Lomefloxacin #3","Lomefloxacin #4","Bleomycin #1","Bleomycin #2","Bleomycin #3","Bleomycin #4","Colistin #1","Colistin #2","Colistin #3","Colistin #4","Minocycline #1","Minocycline #2","Minocycline #3","Minocycline #4","Capreomycin #1","Capreomycin #2","Capreomycin #3","Capreomycin #4","Demeclocycline #1","Demeclocycline #2","Demeclocycline #3","Demeclocycline #4","Nafcillin #1","Nafcillin #2","Nafcillin #3","Nafcillin #4","Cefazolin #1","Cefazolin #2","Cefazolin #3","Cefazolin #4","Enoxacin #1","Enoxacin #2","Enoxacin #3","Enoxacin #4","Nalidixic Acid #1","Nalidixic Acid #2","Nalidixic Acid #3","Nalidixic Acid #4","Chloramphenicol #1","Chloramphenicol #2","Chloramphenicol #3","Chloramphenicol #4","Erythromycin #1","Erythromycin #2","Erythromycin #3","Erythromycin #4","Neomycin #1","Neomycin #2","Neomycin #3","Neomycin #4","Ceftriaxone #1","Ceftriaxone #2","Ceftriaxone #3","Ceftriaxone #4","Gentamicin #1","Gentamicin #2","Gentamicin #3","Gentamicin #4","Potassium Tellurite #1","Potassium Tellurite #2","Potassium Tellurite #3","Potassium Tellurite #4","Cephalothin #1","Cephalothin #2","Cephalothin #3","Cephalothin #4","Kanamycin #1","Kanamycin #2","Kanamycin #3","Kanamycin #4","Ofloxacin #1","Ofloxacin #2","Ofloxacin #3","Ofloxacin #4"]
    PM12 _ -> ["Penicillin G #1","Penicillin G #2","Penicillin G #3","Penicillin G #4","Tetracycline #1","Tetracycline #2","Tetracycline #3","Tetracycline #4","Carbenicillin #1","Carbenicillin #2","Carbenicillin #3","Carbenicillin #4","Oxacillin #1","Oxacillin #2","Oxacillin #3","Oxacillin #4","Penimepicycline #1","Penimepicycline #2","Penimepicycline #3","Penimepicycline #4","Polymyxin B #1","Polymyxin B #2","Polymyxin B #3","Polymyxin B #4","Paromomycin #1","Paromomycin #2","Paromomycin #3","Paromomycin #4","Vancomycin #1","Vancomycin #2","Vancomycin #3","Vancomycin #4","D,L-Serine Hydroxamate #1","D,L-Serine Hydroxamate #2","D,L-Serine Hydroxamate #3","D,L-Serine Hydroxamate #4","Sisomicin #1","Sisomicin #2","Sisomicin #3","Sisomicin #4","Sulfamethazine #1","Sulfamethazine #2","Sulfamethazine #3","Sulfamethazine #4","Novobiocin #1","Novobiocin #2","Novobiocin #3","Novobiocin #4","2,4-Diamino-6,7-Diisopropylpteridine #1","2,4-Diamino-6,7-Diisopropylpteridine #2","2,4-Diamino-6,7-Diisopropylpteridine #3","2,4-Diamino-6,7-Diisopropylpteridine #4","Sulfadiazine #1","Sulfadiazine #2","Sulfadiazine #3","Sulfadiazine #4","Benzethonium Chloride #1","Benzethonium Chloride #2","Benzethonium Chloride #3","Benzethonium Chloride #4","Tobramycin #1","Tobramycin #2","Tobramycin #3","Tobramycin #4","Sulfathiazole #1","Sulfathiazole #2","Sulfathiazole #3","Sulfathiazole #4","5-Fluoroorotic Acid #1","5-Fluoroorotic Acid #2","5-Fluoroorotic Acid #3","5-Fluoroorotic Acid #4","Spectinomycin #1","Spectinomycin #2","Spectinomycin #3","Spectinomycin #4","Sulfamethoxazole #1","Sulfamethoxazole #2","Sulfamethoxazole #3","Sulfamethoxazole #4","L-Aspartic-β-Hydroxamate #1","L-Aspartic-β-Hydroxamate #2","L-Aspartic-β-Hydroxamate #3","L-Aspartic-β-Hydroxamate #4","Spiramycin #1","Spiramycin #2","Spiramycin #3","Spiramycin #4","Rifampicin #1","Rifampicin #2","Rifampicin #3","Rifampicin #4","Dodecyltrimethyl Ammonium Bromide #1","Dodecyltrimethyl Ammonium Bromide #2","Dodecyltrimethyl Ammonium Bromide #3","Dodecyltrimethyl Ammonium Bromide #4"]
    PM13 _ -> ["Ampicillin #1","Ampicillin #2","Ampicillin #3","Ampicillin #4","Dequalinium #1","Dequalinium #2","Dequalinium #3","Dequalinium #4","Nickel Chloride #1","Nickel Chloride #2","Nickel Chloride #3","Nickel Chloride #4","Azlocillin #1","Azlocillin #2","Azlocillin #3","Azlocillin #4","2,2′-Dipyridyl #1","2,2′-Dipyridyl #2","2,2′-Dipyridyl #3","2,2′-Dipyridyl #4","Oxolinic Acid #1","Oxolinic Acid #2","Oxolinic Acid #3","Oxolinic Acid #4","Mercaptopurine #1","Mercaptopurine #2","Mercaptopurine #3","Mercaptopurine #4","Doxycycline #1","Doxycycline #2","Doxycycline #3","Doxycycline #4","Potassium Chromate #1","Potassium Chromate #2","Potassium Chromate #3","Potassium Chromate #4","Cefuroxime #1","Cefuroxime #2","Cefuroxime #3","Cefuroxime #4","5-Fluorouracil #1","5-Fluorouracil #2","5-Fluorouracil #3","5-Fluorouracil #4","Rolitetracycline #1","Rolitetracycline #2","Rolitetracycline #3","Rolitetracycline #4","Cytarabine #1","Cytarabine #2","Cytarabine #3","Cytarabine #4","Geneticin [G418] #1","Geneticin [G418] #2","Geneticin [G418] #3","Geneticin [G418] #4","Ruthenium Red #1","Ruthenium Red #2","Ruthenium Red #3","Ruthenium Red #4","Cesium Chloride #1","Cesium Chloride #2","Cesium Chloride #3","Cesium Chloride #4","Glycine #1","Glycine #2","Glycine #3","Glycine #4","Thallium [I] Acetate #1","Thallium [I] Acetate #2","Thallium [I] Acetate #3","Thallium [I] Acetate #4","Cobalt [II] Chloride #1","Cobalt [II] Chloride #2","Cobalt [II] Chloride #3","Cobalt [II] Chloride #4","Manganese [II] Chloride #1","Manganese [II] Chloride #2","Manganese [II] Chloride #3","Manganese [II] Chloride #4","Trifluoperazine #1","Trifluoperazine #2","Trifluoperazine #3","Trifluoperazine #4","Copper [II] Chloride #1","Copper [II] Chloride #2","Copper [II] Chloride #3","Copper [II] Chloride #4","Moxalactam #1","Moxalactam #2","Moxalactam #3","Moxalactam #4","Tylosin #1","Tylosin #2","Tylosin #3","Tylosin #4"]
    PM14 _ -> ["Acriflavine #1","Acriflavine #2","Acriflavine #3","Acriflavine #4","Furaltadone #1","Furaltadone #2","Furaltadone #3","Furaltadone #4","Sanguinarine #1","Sanguinarine #2","Sanguinarine #3","Sanguinarine #4","Aminacrine #1","Aminacrine #2","Aminacrine #3","Aminacrine #4","Fusaric Acid #1","Fusaric Acid #2","Fusaric Acid #3","Fusaric Acid #4","Sodium Arsenate #1","Sodium Arsenate #2","Sodium Arsenate #3","Sodium Arsenate #4","Boric Acid #1","Boric Acid #2","Boric Acid #3","Boric Acid #4","1-Hydroxy-Pyridine-2-Thione #1","1-Hydroxy-Pyridine-2-Thione #2","1-Hydroxy-Pyridine-2-Thione #3","1-Hydroxy-Pyridine-2-Thione #4","Sodium Cyanate #1","Sodium Cyanate #2","Sodium Cyanate #3","Sodium Cyanate #4","Cadmium Chloride #1","Cadmium Chloride #2","Cadmium Chloride #3","Cadmium Chloride #4","Iodoacetic Acid #1","Iodoacetic Acid #2","Iodoacetic Acid #3","Iodoacetic Acid #4","Sodium Dichromate #1","Sodium Dichromate #2","Sodium Dichromate #3","Sodium Dichromate #4","Cefoxitin #1","Cefoxitin #2","Cefoxitin #3","Cefoxitin #4","Nitrofurantoin #1","Nitrofurantoin #2","Nitrofurantoin #3","Nitrofurantoin #4","Sodium Metaborate #1","Sodium Metaborate #2","Sodium Metaborate #3","Sodium Metaborate #4","Chloramphenicol #1","Chloramphenicol #2","Chloramphenicol #3","Chloramphenicol #4","Piperacillin #1","Piperacillin #2","Piperacillin #3","Piperacillin #4","Sodium Metavanadate #1","Sodium Metavanadate #2","Sodium Metavanadate #3","Sodium Metavanadate #4","Chelerythrine #1","Chelerythrine #2","Chelerythrine #3","Chelerythrine #4","Carbenicillin #1","Carbenicillin #2","Carbenicillin #3","Carbenicillin #4","Sodium Nitrite #1","Sodium Nitrite #2","Sodium Nitrite #3","Sodium Nitrite #4","EGTA #1","EGTA #2","EGTA #3","EGTA #4","Promethazine #1","Promethazine #2","Promethazine #3","Promethazine #4","Sodium Orthovanadate #1","Sodium Orthovanadate #2","Sodium Orthovanadate #3","Sodium Orthovanadate #4"]
    PM15 _ -> ["Procaine #1","Procaine #2","Procaine #3","Procaine #4","Guanidine Hydrochloride #1","Guanidine Hydrochloride #2","Guanidine Hydrochloride #3","Guanidine Hydrochloride #4","Cefmetazole #1","Cefmetazole #2","Cefmetazole #3","Cefmetazole #4","D-Cycloserine #1","D-Cycloserine #2","D-Cycloserine #3","D-Cycloserine #4","EDTA #1","EDTA #2","EDTA #3","EDTA #4","5,7-Dichloro-8-Hydroxy-Quinaldine #1","5,7-Dichloro-8-Hydroxy-Quinaldine #2","5,7-Dichloro-8-Hydroxy-Quinaldine #3","5,7-Dichloro-8-Hydroxy-Quinaldine #4","5,7-Dichloro-8-Hydroxyquinoline #1","5,7-Dichloro-8-Hydroxyquinoline #2","5,7-Dichloro-8-Hydroxyquinoline #3","5,7-Dichloro-8-Hydroxyquinoline #4","Fusidic Acid #1","Fusidic Acid #2","Fusidic Acid #3","Fusidic Acid #4","1,10-Phenanthroline #1","1,10-Phenanthroline #2","1,10-Phenanthroline #3","1,10-Phenanthroline #4","Phleomycin #1","Phleomycin #2","Phleomycin #3","Phleomycin #4","Domiphen Bromide #1","Domiphen Bromide #2","Domiphen Bromide #3","Domiphen Bromide #4","Nordihydroguaiaretic Acid #1","Nordihydroguaiaretic Acid #2","Nordihydroguaiaretic Acid #3","Nordihydroguaiaretic Acid #4","Alexidine #1","Alexidine #2","Alexidine #3","Alexidine #4","Nitrofurazone #1","Nitrofurazone #2","Nitrofurazone #3","Nitrofurazone #4","Methyl Viologen #1","Methyl Viologen #2","Methyl Viologen #3","Methyl Viologen #4","3,4-Dimethoxybenzyl Alcohol #1","3,4-Dimethoxybenzyl Alcohol #2","3,4-Dimethoxybenzyl Alcohol #3","3,4-Dimethoxybenzyl Alcohol #4","Oleandomycin #1","Oleandomycin #2","Oleandomycin #3","Oleandomycin #4","Puromycin #1","Puromycin #2","Puromycin #3","Puromycin #4","CCCP #1","CCCP #2","CCCP #3","CCCP #4","Sodium Azide #1","Sodium Azide #2","Sodium Azide #3","Sodium Azide #4","Menadione #1","Menadione #2","Menadione #3","Menadione #4","2-Nitroimidazole #1","2-Nitroimidazole #2","2-Nitroimidazole #3","2-Nitroimidazole #4","Hydroxyurea #1","Hydroxyurea #2","Hydroxyurea #3","Hydroxyurea #4","Zinc Chloride #1","Zinc Chloride #2","Zinc Chloride #3","Zinc Chloride #4"]
    PM16 _ -> ["Cefotaxime #1","Cefotaxime #2","Cefotaxime #3","Cefotaxime #4","Phosphomycin #1","Phosphomycin #2","Phosphomycin #3","Phosphomycin #4","5-Chloro-7-Iodo-8-Hydroxyquinoline #1","5-Chloro-7-Iodo-8-Hydroxyquinoline #2","5-Chloro-7-Iodo-8-Hydroxyquinoline #3","5-Chloro-7-Iodo-8-Hydroxyquinoline #4","Norfloxacin #1","Norfloxacin #2","Norfloxacin #3","Norfloxacin #4","Sulfanilamide #1","Sulfanilamide #2","Sulfanilamide #3","Sulfanilamide #4","Trimethoprim #1","Trimethoprim #2","Trimethoprim #3","Trimethoprim #4","Dichlofluanid #1","Dichlofluanid #2","Dichlofluanid #3","Dichlofluanid #4","Protamine Sulfate #1","Protamine Sulfate #2","Protamine Sulfate #3","Protamine Sulfate #4","Cetylpyridinium Chloride #1","Cetylpyridinium Chloride #2","Cetylpyridinium Chloride #3","Cetylpyridinium Chloride #4","1-Chloro-2,4-Dinitrobenzene #1","1-Chloro-2,4-Dinitrobenzene #2","1-Chloro-2,4-Dinitrobenzene #3","1-Chloro-2,4-Dinitrobenzene #4","Diamide #1","Diamide #2","Diamide #3","Diamide #4","Cinoxacin #1","Cinoxacin #2","Cinoxacin #3","Cinoxacin #4","Streptomycin #1","Streptomycin #2","Streptomycin #3","Streptomycin #4","5-Azacytidine #1","5-Azacytidine #2","5-Azacytidine #3","5-Azacytidine #4","Rifamycin SV #1","Rifamycin SV #2","Rifamycin SV #3","Rifamycin SV #4","Potassium Tellurite #1","Potassium Tellurite #2","Potassium Tellurite #3","Potassium Tellurite #4","Sodium Selenite #1","Sodium Selenite #2","Sodium Selenite #3","Sodium Selenite #4","Aluminum Sulfate #1","Aluminum Sulfate #2","Aluminum Sulfate #3","Aluminum Sulfate #4","Chromium [III] Chloride #1","Chromium [III] Chloride #2","Chromium [III] Chloride #3","Chromium [III] Chloride #4","Ferric Chloride #1","Ferric Chloride #2","Ferric Chloride #3","Ferric Chloride #4","L-Glutamic Acid γ-Hydroxamate #1","L-Glutamic Acid γ-Hydroxamate #2","L-Glutamic Acid γ-Hydroxamate #3","L-Glutamic Acid γ-Hydroxamate #4","Glycine Hydroxamate #1","Glycine Hydroxamate #2","Glycine Hydroxamate #3","Glycine Hydroxamate #4","Chloroxylenol #1","Chloroxylenol #2","Chloroxylenol #3","Chloroxylenol #4","Sorbic Acid #1","Sorbic Acid #2","Sorbic Acid #3","Sorbic Acid #4"]
    PM17 _ -> ["D-Serine #1","D-Serine #2","D-Serine #3","D-Serine #4","β-Chloro-L-Alanine #1","β-Chloro-L-Alanine #2","β-Chloro-L-Alanine #3","β-Chloro-L-Alanine #4","Thiosalicylate #1","Thiosalicylate #2","Thiosalicylate #3","Thiosalicylate #4","2-Hydroxy-Benzoic Acid #1","2-Hydroxy-Benzoic Acid #2","2-Hydroxy-Benzoic Acid #3","2-Hydroxy-Benzoic Acid #4","Hygromycin B #1","Hygromycin B #2","Hygromycin B #3","Hygromycin B #4","Ethionamide #1","Ethionamide #2","Ethionamide #3","Ethionamide #4","4-Aminopyridine #1","4-Aminopyridine #2","4-Aminopyridine #3","4-Aminopyridine #4","Sulfachloropyridazine #1","Sulfachloropyridazine #2","Sulfachloropyridazine #3","Sulfachloropyridazine #4","Sulfamonomethoxine #1","Sulfamonomethoxine #2","Sulfamonomethoxine #3","Sulfamonomethoxine #4","Oxycarboxin #1","Oxycarboxin #2","Oxycarboxin #3","Oxycarboxin #4","3-Amino-1,2,4-triazole #1","3-Amino-1,2,4-triazole #2","3-Amino-1,2,4-triazole #3","3-Amino-1,2,4-triazole #4","Chlorpromazine #1","Chlorpromazine #2","Chlorpromazine #3","Chlorpromazine #4","Niaproof #1","Niaproof #2","Niaproof #3","Niaproof #4","Compound 48/80 #1","Compound 48/80 #2","Compound 48/80 #3","Compound 48/80 #4","Sodium Tungstate #1","Sodium Tungstate #2","Sodium Tungstate #3","Sodium Tungstate #4","Lithium Chloride #1","Lithium Chloride #2","Lithium Chloride #3","Lithium Chloride #4","D,L-Methionine Hydroxamate #1","D,L-Methionine Hydroxamate #2","D,L-Methionine Hydroxamate #3","D,L-Methionine Hydroxamate #4","Tannic Acid #1","Tannic Acid #2","Tannic Acid #3","Tannic Acid #4","Chlorambucil #1","Chlorambucil #2","Chlorambucil #3","Chlorambucil #4","Cefamandole Nafate #1","Cefamandole Nafate #2","Cefamandole Nafate #3","Cefamandole Nafate #4","Cefoperazone #1","Cefoperazone #2","Cefoperazone #3","Cefoperazone #4","Cefsulodin #1","Cefsulodin #2","Cefsulodin #3","Cefsulodin #4","Caffeine #1","Caffeine #2","Caffeine #3","Caffeine #4","Phenylarsine Oxide #1","Phenylarsine Oxide #2","Phenylarsine Oxide #3","Phenylarsine Oxide #4"]
    PM18 _ -> ["Ketoprofen #1","Ketoprofen #2","Ketoprofen #3","Ketoprofen #4","Sodium Pyrophosphate #1","Sodium Pyrophosphate #2","Sodium Pyrophosphate #3","Sodium Pyrophosphate #4","Thiamphenicol #1","Thiamphenicol #2","Thiamphenicol #3","Thiamphenicol #4","Trifluorothymidine #1","Trifluorothymidine #2","Trifluorothymidine #3","Trifluorothymidine #4","Pipemidic Acid #1","Pipemidic Acid #2","Pipemidic Acid #3","Pipemidic Acid #4","Azathioprine #1","Azathioprine #2","Azathioprine #3","Azathioprine #4","Poly-L-Lysine #1","Poly-L-Lysine #2","Poly-L-Lysine #3","Poly-L-Lysine #4","Sulfisoxazole #1","Sulfisoxazole #2","Sulfisoxazole #3","Sulfisoxazole #4","Pentachlorophenol #1","Pentachlorophenol #2","Pentachlorophenol #3","Pentachlorophenol #4","Sodium m-Arsenite #1","Sodium m-Arsenite #2","Sodium m-Arsenite #3","Sodium m-Arsenite #4","Sodium Bromate #1","Sodium Bromate #2","Sodium Bromate #3","Sodium Bromate #4","Lidocaine #1","Lidocaine #2","Lidocaine #3","Lidocaine #4","Sodium Metasilicate #1","Sodium Metasilicate #2","Sodium Metasilicate #3","Sodium Metasilicate #4","Sodium Metaperiodate #1","Sodium Metaperiodate #2","Sodium Metaperiodate #3","Sodium Metaperiodate #4","Antimony [III] Chloride #1","Antimony [III] Chloride #2","Antimony [III] Chloride #3","Antimony [III] Chloride #4","Semicarbazide Hydrochloride #1","Semicarbazide Hydrochloride #2","Semicarbazide Hydrochloride #3","Semicarbazide Hydrochloride #4","Tinidazole #1","Tinidazole #2","Tinidazole #3","Tinidazole #4","Aztreonam #1","Aztreonam #2","Aztreonam #3","Aztreonam #4","Triclosan #1","Triclosan #2","Triclosan #3","Triclosan #4","Guanazole #1","Guanazole #2","Guanazole #3","Guanazole #4","Myricetin #1","Myricetin #2","Myricetin #3","Myricetin #4","5-Fluoro-5′-Deoxyuridine #1","5-Fluoro-5′-Deoxyuridine #2","5-Fluoro-5′-Deoxyuridine #3","5-Fluoro-5′-Deoxyuridine #4","2-Phenylphenol #1","2-Phenylphenol #2","2-Phenylphenol #3","2-Phenylphenol #4","Plumbagin #1","Plumbagin #2","Plumbagin #3","Plumbagin #4"]
    PM19 _ -> ["Josamycin #1","Josamycin #2","Josamycin #3","Josamycin #4","Gallic Acid #1","Gallic Acid #2","Gallic Acid #3","Gallic Acid #4","Coumarin #1","Coumarin #2","Coumarin #3","Coumarin #4","Methyltrioctylammonium Chloride #1","Methyltrioctylammonium Chloride #2","Methyltrioctylammonium Chloride #3","Methyltrioctylammonium Chloride #4","Harmane #1","Harmane #2","Harmane #3","Harmane #4","2,4-Dinitrophenol #1","2,4-Dinitrophenol #2","2,4-Dinitrophenol #3","2,4-Dinitrophenol #4","Chlorhexidine #1","Chlorhexidine #2","Chlorhexidine #3","Chlorhexidine #4","Umbelliferone #1","Umbelliferone #2","Umbelliferone #3","Umbelliferone #4","Cinnamic Acid [trans] #1","Cinnamic Acid [trans] #2","Cinnamic Acid [trans] #3","Cinnamic Acid [trans] #4","Disulphiram #1","Disulphiram #2","Disulphiram #3","Disulphiram #4","Iodonitro Tetrazolium Violet #1","Iodonitro Tetrazolium Violet #2","Iodonitro Tetrazolium Violet #3","Iodonitro Tetrazolium Violet #4","Phenyl-Methyl-Sulfonyl Fluoride #1","Phenyl-Methyl-Sulfonyl Fluoride #2","Phenyl-Methyl-Sulfonyl Fluoride #3","Phenyl-Methyl-Sulfonyl Fluoride #4","FCCP #1","FCCP #2","FCCP #3","FCCP #4","D,L-α-Lipoic Acid #1","D,L-α-Lipoic Acid #2","D,L-α-Lipoic Acid #3","D,L-α-Lipoic Acid #4","Lawsone #1","Lawsone #2","Lawsone #3","Lawsone #4","Phenethicillin #1","Phenethicillin #2","Phenethicillin #3","Phenethicillin #4","Blasticidin S #1","Blasticidin S #2","Blasticidin S #3","Blasticidin S #4","Sodium Caprylate #1","Sodium Caprylate #2","Sodium Caprylate #3","Sodium Caprylate #4","Lauryl Sulfobetaine #1","Lauryl Sulfobetaine #2","Lauryl Sulfobetaine #3","Lauryl Sulfobetaine #4","Dihydrostreptomycin #1","Dihydrostreptomycin #2","Dihydrostreptomycin #3","Dihydrostreptomycin #4","Hydroxylamine #1","Hydroxylamine #2","Hydroxylamine #3","Hydroxylamine #4","Hexaminecobalt [III] Chloride #1","Hexaminecobalt [III] Chloride #2","Hexaminecobalt [III] Chloride #3","Hexaminecobalt [III] Chloride #4","α-Thioglycerol #1","α-Thioglycerol #2","α-Thioglycerol #3","α-Thioglycerol #4","Polymyxin B #1","Polymyxin B #2","Polymyxin B #3","Polymyxin B #4"]
    PM20 _ -> ["Amitriptyline #1","Amitriptyline #2","Amitriptyline #3","Amitriptyline #4","Apramycin #1","Apramycin #2","Apramycin #3","Apramycin #4","Benserazide #1","Benserazide #2","Benserazide #3","Benserazide #4","Orphenadrine #1","Orphenadrine #2","Orphenadrine #3","Orphenadrine #4","D,L-Propranolol #1","D,L-Propranolol #2","D,L-Propranolol #3","D,L-Propranolol #4","Tetrazolium Violet #1","Tetrazolium Violet #2","Tetrazolium Violet #3","Tetrazolium Violet #4","Thioridazine #1","Thioridazine #2","Thioridazine #3","Thioridazine #4","Atropine #1","Atropine #2","Atropine #3","Atropine #4","Ornidazole #1","Ornidazole #2","Ornidazole #3","Ornidazole #4","Proflavine #1","Proflavine #2","Proflavine #3","Proflavine #4","Ciprofloxacin #1","Ciprofloxacin #2","Ciprofloxacin #3","Ciprofloxacin #4","18-Crown-6-Ether #1","18-Crown-6-Ether #2","18-Crown-6-Ether #3","18-Crown-6-Ether #4","Crystal Violet #1","Crystal Violet #2","Crystal Violet #3","Crystal Violet #4","Dodine #1","Dodine #2","Dodine #3","Dodine #4","Hexachlorophene #1","Hexachlorophene #2","Hexachlorophene #3","Hexachlorophene #4","4-Hydroxycoumarin #1","4-Hydroxycoumarin #2","4-Hydroxycoumarin #3","4-Hydroxycoumarin #4","Oxytetracycline #1","Oxytetracycline #2","Oxytetracycline #3","Oxytetracycline #4","Pridinol #1","Pridinol #2","Pridinol #3","Pridinol #4","Captan #1","Captan #2","Captan #3","Captan #4","3,5-Dinitrobenzoic Acid #1","3,5-Dinitrobenzoic Acid #2","3,5-Dinitrobenzoic Acid #3","3,5-Dinitrobenzoic Acid #4","8-Hydroxyquinoline #1","8-Hydroxyquinoline #2","8-Hydroxyquinoline #3","8-Hydroxyquinoline #4","Patulin #1","Patulin #2","Patulin #3","Patulin #4","Tolylfluanid #1","Tolylfluanid #2","Tolylfluanid #3","Tolylfluanid #4","Troleandomycin #1","Troleandomycin #2","Troleandomycin #3","Troleandomycin #4"]

