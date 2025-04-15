import XMonad
import XMonad.Hooks.SetWMName

main :: IO ()
main = xmonad $ def
     { 
         terminal = "alacritty",
         startupHook = setWMName "LG3D"
     }
