module Output( banner
             , module Output.Pdf
             , module Output.Image
             , module Output.Source ) where

import           Output.Image
import           Output.Pdf
import           Output.Source


banner :: String -> String
banner txt = let prettyBorder = replicate 10 '='
  in mconcat [prettyBorder, " ", txt, " ", prettyBorder, "\n"]
