module Output( banner ) where

banner :: String -> String
banner txt = let prettyBorder = replicate 10 '='
  in mconcat [prettyBorder, " ", txt, " ", prettyBorder, "\n"]

prettyPrint :: a
prettyPrint = undefined
