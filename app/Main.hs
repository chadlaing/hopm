{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Prelude ()
import System.IO (print, IO)
import Control.Monad (mapM)
import System.Directory (getDirectoryContents)
import Data.Functor (fmap)
import ParseKineticFile
import qualified Data.ByteString.Lazy.Char8 as BS


main :: IO ()
main = do
    let theDirectory = "/home/chad/workspace/ecoli_omnilog/data/kinetic/AllEdited"
    allFiles <- getDirectoryContents theDirectory
    allData <- mapM BS.readFile allFiles
    let (a:b) = allData
    print a





