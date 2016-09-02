-- file: CommonFunctions.hs
--import System.IO
module CommonFunctions where 

import System.Process (readProcess)
import qualified Data.String.Utils as SU
import Data.List (findIndices, findIndex, isInfixOf)
import Data.Maybe (fromJust, isJust)

-- functions used to find lines generating an output

firstClosingBracketAfter :: Int -> [String] -> Maybe Int
firstClosingBracketAfter i list = if(isJust index) then Just $ (fromJust index) + length list1 else Nothing
                                     where (list1,list2) = splitAt i list
                                           index = findIndex (\x -> SU.startswith ":}" x) list2

firstOpeningBracketBefore :: Int -> [String] -> Maybe Int
firstOpeningBracketBefore i list = if(isJust index) then Just $ length list2 - 1 - (fromJust index) else Nothing
                                     where (list1,list2) = splitAt (length list - i - 1) (reverse list)
                                           index = findIndex (\x -> SU.startswith ":{" x) list2

lineBelongsToMultiBlock :: Int -> [String] -> Bool
lineBelongsToMultiBlock i list = if isJust firstCBafter
                                   then firstOBbefore == firstOpeningBracketBefore (fromJust firstCBafter) list
                                   else False
                                     where firstOBbefore = firstOpeningBracketBefore i list
                                           firstCBafter = firstClosingBracketAfter i list

prefixesToDetect = ["instance", "import ", "let ", "data ", "where ", "--", "{-#", ":s", ":u", ":l", ":m", ",", "<", "{", "}", "$", ")"]

beginByKeyWord :: String -> [String] -> Bool
beginByKeyWord string keywords = foldr (||) False $ map (\x -> SU.startswith x string) (keywords ++ prefixesToDetect)

isAction :: String -> Bool
isAction string = "<-" `isInfixOf` string

isOutput :: [String] -> [Int]
isOutput strings = findIndices (==False) tests
                    where tests = map (\i -> ((isAction $ tstrings !! i) || (tstrings !! i) == "") || (beginByKeyWord ((tstrings !! i)) prefixesToDetect) || (lineBelongsToMultiBlock i tstrings)) [0..((length tstrings)-1)]
                          tstrings = map SU.lstrip strings
              
                          

-- get the outputs of a script
getGHCIoutputs :: FilePath -> Maybe String -> IO(String)
getGHCIoutputs hsfile packages =
  do
    outputs <- readProcess "ghc" ((if isJust packages then ["-package-db", fromJust packages] else []) ++ ["-dppr-cols140", "-e", ":script " ++ hsfile]) ""
    return ( outputs )



-- read several files 
getFiles :: [FilePath] -> IO([[String]])
getFiles files = 
  do 
    mapM (\file -> fmap lines (readFile file)) files


-- insert something at given positions

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = as ++ (x:bs)
                  where (as,bs) = splitAt n xs

insertAtIndices :: [Int] -> a -> [a] -> [a]
insertAtIndices indices element list | indices == [] = list 
                                    | is == [] = x ++ list2
                                    | otherwise = x ++ (insertAtIndices (map (\j -> j-(length(list1))) is) element list2)  
                                        where x = insertAt i element list1
                                              (list1,list2) = splitAt i list
                                              (i:is) = indices

insertAtIndices2 :: [Int] -> [a] -> [a] -> [a]
insertAtIndices2 indices elements list | indices == [] = list
                                       | is == [] = x ++ list2
                                       | otherwise = x ++ (insertAtIndices2 (map (\j -> j-(length(list1))) is) elements2 list2)  
                                          where x = insertAt i element1 list1
                                                (list1,list2) = splitAt i list
                                                (element1:elements2) = elements
                                                (i:is) = indices


