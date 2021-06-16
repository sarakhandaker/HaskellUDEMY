
func x y z = x + y + z

data Colour = RGB Int Int Int deriving Show

red :: Colour -> Int
red (RGB r _ _) = r
-- lambda equivalent
-- red = /(RGB r _ _) -> r

green :: Colour -> Int
green (RGB _ g _) = g

blue :: Colour -> Int
blue (RGB _ _ b) = b

-- nested patern match
data Pixel = Pixel Int Int Int Colour

pixelRed :: Pixel -> Int
pixelRed (Pixel _ _ _ (RGB r _ _)) = r

data Colour2 = RGB Int Int Int | CMYK Float Float Float Float deriving Show

{-
-- Function declaration pattern match
colourModel :: Colour -> String
colourModel (RGB _ _ _) = "RGB"
colourModel (CMYK _ _ _ _) = "CMYK"
-}

{-
-- case..of pattern match
colourModel :: Colour -> String
colourModel c =
    case c of RGB _ _ _ -> "RGB"
              CMYK _ _ _ _ -> "CMYK"
-}

-- Non-exhaustive pattern match
colourModel :: Colour -> String
colourModel c =
    case c of RGB _ _ _ -> "RGB"

    -- to compile with the non-exhaustive pattern match warming use stack ghc -- -Wincomplete-patterns Colour.hs

main :: IO ()
main = do
    let p = Pixel 100 100 100 (RGB 10 20 30)
    print $ pixelRed p
    print $ func 1 2 3
    print $ RGB 10 20 30
