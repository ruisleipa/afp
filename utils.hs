-- Advanced Functional Programming 2014, Coursework
-- Mikko Paukkonen

module Utils where      
      
elemAtIndex :: Int -> [a] -> Maybe a
elemAtIndex index xs =
    if index < length xs then
        Just (xs !! index)
    else
        Nothing
