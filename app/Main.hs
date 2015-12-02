{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Prelude (Double)
import System.IO (print, IO)
import Control.Monad (mapM, liftM, filterM)
import System.Directory (getDirectoryContents, doesFileExist)
import Data.Functor (fmap)
import Data.List (filter, or, (++), sort)
import ParseKineticFile (splitHeaderData, createExperiment, summaryValue, wells, maxValue)
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
    (a:fileContents) <- mapM readFile filteredFiles
    let summarizedData = createExperiment $ T.breakOn "Hour" a
    print $ sort $ fmap summaryValue $ (HM.elems . wells) summarizedData






