module Resource where

import Graphics.Image

data Resource = Iron | Copper | Coal | Water | Stone | None | Mark Int | Unknown Word8 Word8 Word8
  deriving (Eq, Show)

abbrevShow :: Resource -> String
abbrevShow Iron = "I"
abbrevShow Copper = "C"
abbrevShow Coal = "O"
abbrevShow Water = "W"
abbrevShow Stone = "S"
abbrevShow None = " "
abbrevShow (Mark _) = "M"
abbrevShow (Unknown r g b) = show (r, g, b)

determineResource :: Pixel RGB Word8 -> Resource
determineResource (PixelRGB 0 0 0) = Coal
determineResource (PixelRGB 45 55 0) = None
determineResource (PixelRGB 51 83 95) = Water
determineResource (PixelRGB 203 97 53) = Copper
determineResource (PixelRGB 104 132 146) = Iron
determineResource (PixelRGB r g b) = Unknown r g b
