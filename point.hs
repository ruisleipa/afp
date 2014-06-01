-- Advanced Functional Programming 2014, Coursework
-- Mikko Paukkonen

module Point where

import Data.List

data Point = Point {
    x :: Integer,
    y :: Integer
} deriving (Show)

instance Eq Point where
    a == b = (x a) == (x b) && (y a) == (y b)

distance :: Point -> Point -> Float
distance a b =
    let
        disp_x = (x a) - (x b)
        disp_y = (y a) - (y b)
    in
        sqrt (fromIntegral ((disp_x ^ 2) + (disp_y ^ 2)))
