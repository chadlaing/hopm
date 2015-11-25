{-# LANGUAGE OverloadedStrings  #-}

module ParseKineticFile
(
getHeader
) where

import qualified Data.Text.Lazy as T

getHeader :: T.Text -> (T.Text, T.Text)
getHeader = T.breakOn "Hour"