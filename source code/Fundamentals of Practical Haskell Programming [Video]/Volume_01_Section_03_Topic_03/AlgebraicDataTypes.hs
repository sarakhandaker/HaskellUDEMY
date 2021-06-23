--SIMPLE SUM TYPE

module ColourSimpleSumType (Colour) where

-- use data keyword
-- red green and blue are dat contructors (where you would expect a value)
-- colour is a type constructor

data Colour = Red | Green | Blue deriving Show



-- EQUIVALENT IN C++
-- enum class Colour
-- {
--     Red,
--     Green,
--     Blue
-- };

--SIMPLE PRODUCT TYPE

module ColourRGB (Colour) where

data Colour = RGB Int Int Int deriving Show

-- EQUIVALENT IN C++
-- struct Colour
-- {
--     int red;
--     int green;
--     int blue;
-- };

-- RECORD Syntax (uses braces)

module ColourRGBRecord (Colour) where

data Colour = RGB
    { red :: Int
    , green :: Int
    , blue :: Int
    } deriving Show

    -- red blue and green are "accsessors"
    -- can use record update syntax to make new values (not to update exisiting)

-- COMBINATION OF SUM AND PRODUCT TYPE

module ColourRGBCMYK (Colour) where

data Colour = RGB Int Int Int | CMYK Float Float Float Float deriving Show