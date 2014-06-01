-- Advanced Functional Programming 2014, Coursework
-- Mikko Paukkonen

module Stop where

import Data.List
import Data.Maybe
import Text.Read
import Point
import Utils

data Stop = Stop {
    number :: Integer,
    point :: Point
} deriving (Show)

parse_stop :: String -> Maybe Stop
parse_stop s =
    let
        params = words s
        cmd_s = elemAtIndex 0 params
        number_s = elemAtIndex 1 params
        x_s = elemAtIndex 2 params
        y_s = elemAtIndex 3 params
    in
        if all isJust [cmd_s, number_s, x_s, y_s] then
            let
                cmd = fromJust cmd_s
                number = readMaybe (fromJust number_s) :: Maybe Integer
                x = readMaybe (fromJust x_s) :: Maybe Integer
                y = readMaybe (fromJust y_s) :: Maybe Integer
            in
                if cmd == "stop" && all isJust [number, x, y] then
                    Just Stop
                    {
                        number = fromJust number, 
                        point = Point
                        {
                            x = fromJust x,
                            y = fromJust y
                        }
                    }
                else
                    Nothing
        else
            Nothing

parse_stops :: [String] -> [Maybe Stop]
parse_stops rows = map parse_stop rows

find_stops_within_distance :: Point -> [Stop] -> Float -> Maybe Stop
find_stops_within_distance location stops dist = 
    let
        nearest = minimumBy (\a b -> compare (distance location (point a)) (distance location (point b))) stops
    in
        if (distance location (point nearest)) <= dist then        
            Just nearest
        else
            Nothing    

