-- Advanced Functional Programming 2014, Coursework
-- Mikko Paukkonen

module Line where

import Data.Maybe
import Text.Read
import Utils

data Line = Line {
    number :: Integer,
    stops1 :: [Integer],
    stops2 :: [Integer]
} deriving (Show)

parse_line :: String -> Maybe Line
parse_line s =
    let
        params = words s
        cmd_s = elemAtIndex 0 params
        number_s = elemAtIndex 1 params
        stops1_s = elemAtIndex 2 params
        stops2_s = elemAtIndex 3 params
    in
        if all isJust [cmd_s, number_s, stops1_s, stops2_s] then
        let
            cmd = fromJust cmd_s
            number = readMaybe (fromJust number_s) :: Maybe Integer
            stops1 = readMaybe (fromJust stops1_s) :: Maybe [Integer]
            stops2 = readMaybe (fromJust stops2_s) :: Maybe [Integer]
        in
            if cmd == "line" && all isJust [stops1, stops2] && isJust number then
                Just Line
                {
                    number = fromJust number,
                    stops1 = fromJust stops1,
                    stops2 = fromJust stops2
                }
        else
            Nothing
    else
        Nothing
            

parse_lines :: [String] -> [Maybe Line]
parse_lines rows = map parse_line rows

