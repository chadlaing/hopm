{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Prelude (Double)
import System.IO (print, IO)
import Control.Monad (mapM, liftM, filterM, mapM_)
import System.Directory (getDirectoryContents, doesFileExist)
import Data.Functor (fmap)
import Data.List (filter, or, (++), sort, take)
import ParseKineticFile (createPMResultTable, summarizeGroup, splitHeaderData, createListOfExperiment, summaryValue, wells, maxValue, groupExperimentBy, otype, name, freeSummaryValues)
import Data.Function ((.), ($))
import Data.Text.Lazy.IO (readFile, putStrLn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as T
import Data.Tuple (fst, snd)
import Data.Tuple.Select (sel3)


main :: IO ()
main = do
    let theDirectory = "/home/chad/workspace/ecoli_omnilog/data/kinetic/AllEdited/"
    allFiles <- getDirectoryContents theDirectory
    filteredFiles <- filterM doesFileExist $
                        fmap (theDirectory ++) allFiles
    fileContents <- mapM readFile filteredFiles
    let summarizedData = createListOfExperiment fileContents
    let groupedData = groupExperimentBy name summarizedData
    let condensedGroupedData = summarizeGroup groupedData
    --let resultTable = createPMResultTable condensedGroupedData
    let freeValues = freeSummaryValues condensedGroupedData
    mapM_ putStrLn $ fmap (\(a,b,c) -> T.concat[a, "\t", b, "\t", c]) freeValues


    --print $ sort $ fmap summaryValue $ (HM.elems . wells) c






