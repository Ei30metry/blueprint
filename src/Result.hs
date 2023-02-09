module Result ( banner
             , module Result.Pdf
             , module Result.Image
             , module Result.Source ) where

import           Result.Image
import           Result.Pdf
import           Result.Source


banner :: String -> String
banner txt = let prettyBorder = replicate 10 '='
  in mconcat [prettyBorder, " ", txt, " ", prettyBorder, "\n"]
