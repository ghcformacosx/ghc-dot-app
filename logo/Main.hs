-- #!/usr/bin/env runhaskell
{-

This builds a full icon set from logo.svg using Inkscape

-}
module Main (main) where

import Data.List ( intercalate )
import Control.Applicative ( (<$>), (<*>) )
import System.Directory ( findExecutable )
import System.Process ( callProcess, readProcess )
import Control.Monad ( forM_ )
data Rect = Rect { rX, rY, rW, rH :: !Double } deriving (Show, Eq)

main :: IO ()
main = makeLogos "logo.svg" outprefix =<< findInkscape
  where outprefix = "../GHC/GHC/Images.xcassets/AppIcon.appiconset/icon_"

callProc :: String -> [String] -> IO ()
callProc p args = do
  putStrLn $ unwords [p, unwords args]
  callProcess p args

makeLogos :: String -> String -> String -> IO ()
makeLogos logo outprefix inkscape = do
  r <- logoSize logo inkscape
  let dpi desired = show $ 90 * desired / rW r
      area = intercalate ":" $ map (show . ($ r))
        [ rX
        , rY
        , (+) <$> rX <*> rW
        , (+) <$> rY <*> rH]
      out f w =
        callProc inkscape
          [ "--without-gui"
          , "--export-dpi=" ++ dpi w
          , "--export-area=" ++ area
          , "--export-png=" ++ f
          , logo ]
  forM_ sizes $ \size -> do
    let sz = show (round size :: Int)
    out (concat [outprefix, sz, "x", sz, ".png"]) size
    out (concat [outprefix, sz, "x", sz, "@2x.png"]) (2 * size)

findInkscape :: IO String
findInkscape = maybe appInkscape id <$> findExecutable "inkscape"
  where
    appInkscape = "/Applications/Inkscape.app/Contents/Resources/bin/inkscape"

logoSize :: String -> String -> IO Rect
logoSize logo inkscape = pageRect <$> rd "-X" <*> rd "-Y" <*> rd "-W" <*> rd "-H"
  where
    rd arg = read <$> readProcess inkscape [ "--without-gui", arg, logo ] ""

sizes :: [ Double ]
sizes = [ 16, 32, 128, 256, 512 ]

pageRect :: Double -> Double -> Double -> Double -> Rect
pageRect x y w h = Rect x' y' w' w'
  where
    x' = x - xmargin
    y' = y - ymargin
    w' = w + 2 * xmargin
    xmargin = (4 / 128) * w
    ymargin = (w' - h) / 2
