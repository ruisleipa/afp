Advanced Functional Programming 2014, Coursework
Mikko Paukkonen

The program begins by loading the provided data files. Read errors are catched with try. Try in the case of readFile provides an Either with the content of the file or the exception that occured. To keep most of the program pure the part doing IO has been kept minimal. Reading files and processing user commands only do IO in a couple of functions. Actual processing such as parsing the positions or stops is done in a pure fashion. Maybe is used extensively to signal error conditions. For example parse_stop on encountering an invalid stop will return Nothing and Just Stop when the parse is successful. The main function reports an error is some file couldn't be loaded or some data item had an error. In the latter case however the program will do its best to answer queries with the data that was successfully read. Efforts have been made to use higher order functions as much as possible. Command dispathing is done using a map. Commands simply get a list of their arguments and return a resulting list of strings to display to the user.

For the different types of data there are three data types: Line, Position and Stop. The types have been implemented as records. This is done to provide names for the different fields to make development less error-prone. Point has also been implemented. Point facilitates easy calculation of distances and comparison for equality of two positions. The program has been divided into separate modules to make the organization of the code a bit cleaner.

A little helper function was made for safe array indexing. elemAtIndex returns Nothing if the index is not valid. This is useful when dealing with user input that has to be checked for validity.

For detecting a stopping bus the positions are sorted by first the vehicle_id and then by the time stamp. The the positions are grouped by the locations. After that it is easy to detect a stopped bus by seeing whether the last and first positions differ by at least one second. To show the stop schedule the positions that are involved in stopping are gathered and it is then determined if the stop happened sufficiently close to the stop. Changes are calculated by combining all the departures and arrivals of the stop and then seeing if they fit the given constraints. The fastestroute command has not been implemented.


