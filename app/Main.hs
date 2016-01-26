{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Prelude (Double)
import System.IO (print, IO)
import Control.Monad (mapM, liftM, filterM)
import System.Directory (getDirectoryContents, doesFileExist)
import Data.Functor (fmap)
import Data.List (filter, or, (++), sort, take)
import ParseKineticFile (summarizeGroup, splitHeaderData, createListOfExperiment, summaryValue, wells, maxValue, groupExperimentBy, otype, name)
import Data.Function ((.), ($))
import Data.Text.Lazy.IO (readFile)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as T


main :: IO ()
main = do
    let theDirectory = "/home/chad/workspace/ecoli_omnilog/data/kinetic/AllEdited/"
    allFiles <- getDirectoryContents theDirectory
    filteredFiles <- filterM doesFileExist $
                        fmap (theDirectory ++) allFiles
    fileContents <- mapM readFile filteredFiles
    let summarizedData = createListOfExperiment (take 10 fileContents)
    let groupedData = groupExperimentBy name summarizedData
    let condensedGroupedData = summarizeGroup groupedData
    let cKeys = HM.keys condensedGroupedData
    print $ fmap (\x -> HM.lookup x condensedGroupedData) cKeys
    print condensedGroupedData
    --print $ sort $ fmap summaryValue $ (HM.elems . wells) c






