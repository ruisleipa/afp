-- Advanced Functional Programming 2014, Coursework
-- Mikko Paukkonen

module Position where

import Data.List
import Data.Maybe
import Text.Read
import Point
import Stop
import Utils

data Position = Position {
    time :: Integer,
    vehicle_number :: Integer,
    line_number :: Integer,
    direction :: Integer,
    point :: Point.Point
} deriving (Show)

parse_position :: String -> Maybe Position
parse_position s =
    let
        params = words s
        cmd_s = elemAtIndex 0 params
        time_s = elemAtIndex 1 params
        vehicle_number_s = elemAtIndex 2 params
        line_number_s = elemAtIndex 3 params
        direction_s = elemAtIndex 4 params
        x_s = elemAtIndex 5 params
        y_s = elemAtIndex 6 params
    in
        if all isJust [cmd_s, time_s, vehicle_number_s, line_number_s, direction_s, x_s, y_s] then
            let
                cmd = fromJust cmd_s
                time = readMaybe (fromJust time_s) :: Maybe Integer
                vehicle_number = readMaybe (fromJust vehicle_number_s) :: Maybe Integer
                line_number = readMaybe (fromJust line_number_s) :: Maybe Integer
                direction = readMaybe (fromJust direction_s) :: Maybe Integer
                x = readMaybe (fromJust x_s) :: Maybe Integer
                y = readMaybe (fromJust y_s) :: Maybe Integer
            in
                if cmd == "pos" && all isJust [time, vehicle_number, line_number, direction, x, y] then
                    Just Position
                    {
                        time = fromJust time, 
                        vehicle_number = fromJust vehicle_number, 
                        line_number = fromJust line_number, 
                        direction = fromJust direction, 
                        Position.point = Point.Point
                        {
                            Point.x = fromJust x, 
                            Point.y = fromJust y
                        }
                    }
                else
                    Nothing
        else
            Nothing
     

compare_position :: Position -> Position -> Ordering
compare_position a b = 
    case compare (time a) (time b) of
        EQ -> compare (vehicle_number a) (vehicle_number b)
        x -> x    

stopschedule :: Stop -> Float -> [[Position]] -> [(Integer, Integer)]
stopschedule stop dist stopped_positions =    
    map (\x -> ((time x), (line_number x))) $
    get_departures stop dist stopped_positions
        
get_departures :: Stop -> Float -> [[Position]] -> [Position]
get_departures stop dist stopped_positions =    
    sortBy compare_position $
    filter (\x -> (distance (Position.point x) (Stop.point stop)) <= dist) $
    map (\x -> last x) stopped_positions
    
get_arrivals :: Stop -> Float -> [[Position]] -> [Position]
get_arrivals stop dist stopped_positions =    
    sortBy compare_position $
    filter (\x -> (distance (Position.point x) (Stop.point stop)) <= dist) $
    map (\x -> head x) stopped_positions

changes :: Stop -> Float -> Integer -> Integer -> [[Position]] -> [(Integer, Integer, Integer, Integer)]    
changes stop dist min_time max_time stopped_positions =
    let
        stop_arrivals = get_arrivals stop dist stopped_positions
        stop_departures = get_departures stop dist stopped_positions
    in
        map (\(arrival, departure) -> ((Position.time arrival), (Position.line_number arrival), (Position.time departure), (Position.line_number departure))) $
        filter (\(arrival, departure) ->
            let
                interval = (Position.time departure) - (Position.time arrival)
            in
                interval >= min_time && interval <= max_time && interval > 0 && (Position.line_number departure) /= (Position.line_number arrival)
        ) $
        [(a, d) | a <- stop_arrivals, d <- stop_departures]   
        
compare_position_for_stopping :: Position -> Position -> Ordering
compare_position_for_stopping a b = 
    case compare (vehicle_number a) (vehicle_number b) of
        EQ -> compare (time a) (time b)
        x -> x    
    
find_stopped_positions :: [Position] -> [[Position]]
find_stopped_positions ps = 
    filter (\x -> ((time (last x)) - (time (head x))) >= 1) $
    filter (\x -> (length x) > 1) $
    groupBy (\a b -> (Position.point a) == (Position.point b)) $
    sortBy compare_position_for_stopping ps
      
parse_positions :: [String] -> [Maybe Position]
parse_positions rows = map parse_position rows

