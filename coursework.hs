-- Advanced Functional Programming 2014, Coursework
-- Mikko Paukkonen

import Control.Monad
import System.IO
import System.Environment
import Data.Maybe
import Text.Read
import Data.List
import Control.Exception
import qualified Data.Map.Strict as Map
import Stop
import Line
import Position
import Utils

command_stopschedule :: [Stop] -> [[Position]] -> [String] -> [String]
command_stopschedule stops stopped_positions params =
    let
        stop_number_s = elemAtIndex 0 params 
        dist_s = elemAtIndex 1 params 
        usage = ["Usage: stopschedule <stop-no> <dist>"]
    in
        if all isJust [stop_number_s, dist_s] then
            let
                stop_number = readMaybe (fromJust stop_number_s) :: Maybe Integer
                dist = readMaybe (fromJust dist_s) :: Maybe Float
            in
                if isJust stop_number && isJust dist then
                    let                        
                        stop = find (\x -> (Stop.number x) == (fromJust stop_number)) stops
                    in   
                        case stop of
                            Nothing -> ["No departures"]
                            Just s -> 
                                let
                                    deps = stopschedule s (fromJust dist) stopped_positions
                                in
                                    if length deps > 0 then 
                                        map (\(t, l) -> show t ++ " " ++ show l) deps
                                    else
                                        ["No departures"]
                else
                    usage
        else
            usage
           
command_changes :: [Stop] -> [[Position]] -> [String] -> [String]
command_changes stops stopped_positions params =
    let
        stop_number_s = elemAtIndex 0 params 
        dist_s = elemAtIndex 1 params 
        min_time_s = elemAtIndex 2 params 
        max_time_s = elemAtIndex 3 params 
        usage = ["Usage: changes <stop-no> <dist> <min-time> <max-time>"]
    in
        if all isJust [stop_number_s, dist_s, min_time_s, max_time_s] then
            let
                stop_number_m = readMaybe (fromJust stop_number_s) :: Maybe Integer
                dist_m = readMaybe (fromJust dist_s) :: Maybe Float
                min_time_m = readMaybe (fromJust min_time_s) :: Maybe Integer
                max_time_m = readMaybe (fromJust max_time_s) :: Maybe Integer
            in
                if all isJust [stop_number_m, min_time_m, max_time_m] && isJust dist_m then
                    let                        
                        stop = find (\x -> (Stop.number x) == (fromJust stop_number_m)) stops
                        dist = fromJust dist_m
                        min_time = fromJust min_time_m
                        max_time = fromJust max_time_m
                    in   
                        case stop of
                            Nothing -> ["No changes"]
                            Just s -> 
                                map (\(a, b, c, d) -> show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d) $                                
                                changes s dist min_time max_time stopped_positions
                else
                    usage
        else
            usage
            
command_fastestroute :: [Stop] -> [[Position]] -> [String] -> [String]
command_fastestroute stops stopped_positions params =
    ["Not implemented"]

process_command :: [String] -> Map.Map String ([String] -> [String]) -> [String]
process_command params commands =
    if (length params) > 0 
        then                
            let
                command_name = head params
                command_params = drop 1 params
                function = Map.lookup command_name commands      
            in
                case function of
                    Just x -> x command_params
                    Nothing -> ["No such command."]
        else
            [""]
            
process_line :: Map.Map String ([String] -> [String]) -> IO ()
process_line commands =
    do
        finished <- isEOF
        unless finished (do
            line <- getLine
            let params = words line
            mapM_ putStrLn (process_command params commands)
            process_line commands)     

safe_read_file :: String -> IO (Maybe String)
safe_read_file file_name =
    do
        result <- try $ readFile file_name
        case result of
            Left (SomeException _) -> return Nothing
            Right contents -> return (Just contents)

process_content :: Maybe String -> [String]
process_content c =
    case c of
        Just x -> lines x
        Nothing -> []
            
main :: IO ()
main =
    do
        [stopfilename, linefilename, positionfilename] <- getArgs
        stop_contents <- safe_read_file stopfilename
        line_contents <- safe_read_file linefilename
        position_contents <- safe_read_file positionfilename
        let stop_file = process_content stop_contents
        let line_file = process_content line_contents
        let position_file = process_content position_contents
        let stops_maybe = parse_stops stop_file
        let lines_maybe = parse_lines line_file
        let positions_maybe = parse_positions position_file        
        let stops = catMaybes stops_maybe
        let lines = catMaybes lines_maybe
        let positions = catMaybes positions_maybe
        let stopped_positions = find_stopped_positions positions
        let commands = Map.fromList [("stopschedule", (command_stopschedule stops stopped_positions)), ("changes", (command_changes stops stopped_positions)), ("fastestroute", (command_fastestroute stops stopped_positions))]
        if (not (all isJust [stop_contents, line_contents, position_contents])) || not (all isJust stops_maybe) || not (all isJust lines_maybe) || not (all isJust positions_maybe) then
            putStrLn "There were errors in the data."
        else
            return ()            
        process_line commands
