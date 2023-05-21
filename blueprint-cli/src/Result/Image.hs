module Result.Image where

-- import           App                       ( BluePrint )

-- import           Data.ByteString           ()
-- import           Data.Tree                 ( Tree (..) )

-- import           Diagrams.Backend.SVG      ( SVG, renderSVG )
-- import           Diagrams.Prelude          ( Any, QDiagram, V2, black, centerXY,
--                                              circle, fc, pad, text, white, with,
--                                              (#), (&), (.~), (~~), ellipseXY )
-- import           Diagrams.Size             ( SizeSpec )
-- import           Diagrams.TwoD.Layout.Tree ( renderTree, slHSep, slVSep,
--                                              symmLayout' )
-- import           Diagrams.TwoD.Size        ( mkHeight )

-- import           Types.AST                 ( BluePrintAST (..) )

-- -- createImage' :: forall b n m. (Monoid' m, Floating n, Ord n) => Tree Char -> QDiagram b V2 n Any
-- createImage' :: Tree String -> QDiagram SVG V2 Double Any
-- createImage' ast = renderTree ((<> circle 1.2 # fc white) . text)
--                               (~~)
--                               (symmLayout' (with & slHSep .~ 5 & slVSep .~ 5) ast) # centerXY # pad 1.1


-- createImage :: BluePrintAST String -> IO ()
-- createImage = renderSVG "test.svg" sizeSp . createImage' . unBAST
--   where sizeSp :: SizeSpec V2 Double
--         sizeSp = mkHeight 200
