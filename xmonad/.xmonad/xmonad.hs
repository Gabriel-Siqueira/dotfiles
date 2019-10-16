import XMonad
import System.Posix.Unistd

main :: IO ()
main = do
  host <- fmap nodeName getSystemID
  xmonad $ def
    { terminal    = "termite"
    , modMask     = mod4Mask
    }
