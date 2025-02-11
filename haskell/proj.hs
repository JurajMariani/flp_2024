-- File:    proj.hs
-- Project: FLP Project: Decision Trees in Haskell
-- Author:  xmaria03
--          Juraj Mariani, <xmaria03@stud.fit.vutbr.cz>
-- Date:    31/3/2024

import System.Environment (getArgs)
import System.IO (openFile, hClose, IOMode(ReadMode), Handle, hIsEOF, hGetLine)
import GHC.Float (int2Float)
import Data.List (sortBy, nub)
import Data.Ord (comparing)

-- Function to read lines from a file and store them in a list
readLines :: FilePath -> IO [String]
readLines filePath = do
    fileHandle <- openFile filePath ReadMode
    liness <- readLinesRecursive fileHandle []
    hClose fileHandle
    return liness
  where
    readLinesRecursive :: Handle -> [String] -> IO [String]
    readLinesRecursive handle acc = do
        eof <- hIsEOF handle
        if eof
            -- Here I reverse the accumulator to preserve the original order
            then return (reverse acc)
            else do
                line <- hGetLine handle
                readLinesRecursive handle (line:acc)

main :: IO ()
main = do
    args <- getArgs
    case args of
--      The first option - Classification Task
        ["-1", treeFilePath, classFilePath] -> do

            treeLines <- readLines treeFilePath
            classLines <- readLines classFilePath
            
            -- The reversed array of Trees is going to be fused to form one tree
            -- The idea is that an ordered tree (as provided in tests) can be stored to a list
            -- as disconnected nodes/leaves
            -- Based on the assumtion of order we can reconstruct the tree from behind:
            -- If the third element of the REVERSED list is a node (Node i f EmptyTree EmptyTree):
            --      we can reduce these three elements as such: Node i f (first element of list) (second element of list)
            -- Otherwise we repeat this process with the list offset by one (second, third and fourth elements) ...
            let tree = fuseTree (reverse (transformToTree treeLines))
            putStrLn $ (joinStrings (classifyNewData tree classLines) "\n")

--      The second option - Tree Training Task
        ["-2", trainDataFilePath] -> do
            trainLines <- readLines trainDataFilePath
            let trainArr = linesToValsAndClasses (map (replace ',' ' ') trainLines)
            putStrLn $ show (trainTree trainArr)
        _ -> putStrLn "Usage: flp-fun ([-1 treeFilePath classificationFilePath] | [-2 trainingFilePath])"

-- Functions for the SECOND TASK

-- Transforms the input lines to a more suitable format: tuple ([features],Class)
linesToValsAndClasses :: [String] -> [([Float],String)]
linesToValsAndClasses [] = []
linesToValsAndClasses (l:ls) = (((transformToFloatArr (take i lineArr)),(head (drop i lineArr))):(linesToValsAndClasses ls)) where
    lineArr = words l
    i = (length lineArr) - 1

-- Function to extract only the class for each feature vector
onlyClasses :: [([Float],String)] -> [String]
onlyClasses d = map snd d

-- Custom function for sorting by a given feature index
sortExtractionFunction :: Int -> ([Float],String) -> Float
sortExtractionFunction i (f,_) = head (drop i f)

-- Calculates the split score for each "point"
-- Takes data and index of split value
-- Returns calculated split score (Weighted gini index)
splitScore :: [String] -> Int -> Float
splitScore cs svi = ((((llen / len) * (giniIndex larray))) + ((rlen / len) * (giniIndex rarray))) where
    larray = take svi cs
    rarray = drop svi cs
    len = int2Float (length cs)
    llen = int2Float (length larray)
    rlen = int2Float (length rarray)

-- Function that traines a decision tree using provided data
trainTree :: [([Float],String)] -> Tree
trainTree d
    | (length dataLabels) == 0 = EmptyTree
    | (length dataLabels) == 1 = Leaf (head dataLabels)
    | otherwise = Node col_idx flt (trainTree (take row_index sd)) (trainTree (drop row_index sd))
    where
        dataLabels = nub (onlyClasses d)
        ret = minSplitIndex d
        col_idx = snd ret
        row_index = fst ret
        sd = sortBy (comparing (sortExtractionFunction col_idx)) d
        lflt = head (drop col_idx (fst (head (drop (row_index-1) sd))))
        rflt = head (drop col_idx (fst (head (drop (row_index) sd))))
        flt = (lflt + rflt) / 2.0

-- Function that calculates the minimal split score and returns the index of said minimum (line row, feature index)
-- IN: Data, OUT: min row, col indices
minSplitIndex :: [([Float],String)] -> (Int,Int)
minSplitIndex [] = (snd (fst x),snd x) where
    x = minSplitIndex' [] 0
minSplitIndex (d:ds) = (snd (fst x),snd x) where
    x = minSplitIndex' (d:ds) ((length (fst d))-1)

-- Calculation of a minimal split index for a given feature
-- IN: Data, Feature index, OUT: Min split score and it's indices
-- Typical use: Accepts a dataset and the length of the feature vector
--              Function traverses the feature vector, applying the function minSplitInColumnIndex
minSplitIndex' :: [([Float],String)] -> Int -> ((Float,Int),Int)
minSplitIndex' d 0 = ((minSplitInColumnIndex d 0),0)
minSplitIndex' d i = ltTuples ((minSplitInColumnIndex d i),i) (minSplitIndex' d (i-1))

-- Calculation of a minimal split index for a given feature
-- This function sorts the dataset by given feature (by index) prior to the calculation
-- IN: Data, Feature index, OUT: min row index for feature
minSplitInColumnIndex :: [([Float],String)] -> Int -> (Float,Int)
minSplitInColumnIndex dta col = minSplitInColumnIndex' sted (length sted) where
    sted = sortBy (comparing (sortExtractionFunction col)) dta

-- Custom comparison for nested tuple
ltTuples :: (Ord a) => ((a,b),c) -> ((a,b),c) -> ((a,b),c)
ltTuples ((x,y),z) ((xx,yy),zz)
    | x < xx = ((x,y),z)
    | otherwise = ((xx,yy),zz)

-- Custom comparison for tuples (comparison only by the first value)
ltTuple :: (Ord a) => (a,b) -> (a,b) -> (a,b)
ltTuple (x,y) (z,w)
    | x < z = (x,y)
    | otherwise = (z,w)

-- Perform a calculation of split value on a specified row
-- IN: Data, Row, OUT: Split value, index of min split value (row)
-- Typical use: Accepts a dataset (sorted by a feature *optional*) and the length of the dataset
--              Function traverses the dataset downward, remembering only the minimal split score information
minSplitInColumnIndex' :: [([Float],String)] -> Int -> (Float,Int)
minSplitInColumnIndex' dta 0 = (splitScore (onlyClasses dta) 0,0)
minSplitInColumnIndex' dta i = ltTuple ((splitScore (onlyClasses dta) i),i) (minSplitInColumnIndex' dta (i-1))

-- Takes the whole array of Classes from training data and calculates the GINI index
giniIndex :: [String] -> Float
giniIndex c = 1 - (giniIndexAdv c (nub c))

-- Takes data (only classes) and a list of unique classes
giniIndexAdv :: [String] -> [String] -> Float
giniIndexAdv _ [] = 0.0
giniIndexAdv c (u:us) = ((giniProportion c u) ** 2.0) + (giniIndexAdv c us)

-- giniProportion calculates the proportion of a given class (u as in unique) to the whole list of classes (c) present in the dataset
giniProportion :: [String] -> String -> Float
giniProportion c u = ((filteredCount c u) / (int2Float (length c))) where
    filteredCount :: [String] -> String -> Float
    filteredCount [] _ = 0.0
    filteredCount (cc:cs) uu
        | cc == uu = 1.0 + filteredCount cs uu
        | otherwise = filteredCount cs uu

-- End of Functions for the SECOND TASK

-- Functions for the FIRST TASK

-- Join strings and separate by delimeter
-- IN: String array, delim, OUT: joint strings
joinStrings :: [String] -> String -> String
joinStrings [] _ = []
joinStrings (x:[]) _ = x
joinStrings (x:xs) delim = x ++ delim ++ (joinStrings xs delim)

-- Perform the classification for feature vector still in string form
-- The feature vector is extracted within
-- IN: Tree, feature vector in string form from file, OUT: Class names for given vectors
classifyNewData :: Tree -> [String] -> [String]
classifyNewData _ [] = []
classifyNewData t (l:ls) = ((classify t (transformToFloatArr (words (replace ',' ' ' l)))):(classifyNewData t ls))

-- Perform classification for a feature vector
-- Returns the Classification of given vector
classify :: Tree -> [Float] -> String
classify EmptyTree _ = ""
classify (Leaf cls) _ = cls
classify (Node i f l r) fa
    | last (take (i+1) fa) > f = classify r fa
    | otherwise = classify l fa

-- Transformation of a list of values in string form to float form
-- Demonstration: IN: "3.14159":("2.71828":("1.61803":[])), OUT: 3.14159:(2.71828:(1.61803:[]))
transformToFloatArr :: [String] -> [Float]
transformToFloatArr [] = []
transformToFloatArr (f:fs) = ((read f):(transformToFloatArr fs))

-- Given an input of a list of disconnected Tree elements, the function performs the fuseTree' operation
-- until only one Tree remains
fuseTree :: [Tree] -> Tree
fuseTree t = if (length fused) == 1
                then head fused
                else fuseTree fused
            where
                fused = fuseTree' t

-- Function takes three Tree elements from the list and if a valid sequence is found
-- they are fused into one and returned to the list
fuseTree' :: [Tree] -> [Tree]
fuseTree' [] = []
fuseTree' (t:ta) = if (length (t:ta)) >= 3
                    then 
                        case (take 3 (t:ta)) of
                            [ln1, ln2, (Node i f EmptyTree EmptyTree)] -> ((Node i f ln2 ln1):(drop 2 ta))
                            arr -> if (length arr) == 3 
                                then (t:(fuseTree' ta))
                                else error "Unable to fuse Tree"
                    else error "Unable to fuse Tree"
    
-- Interpretation of input file lines into tree segments
transformToTree :: [String] -> [Tree]
transformToTree [] = []
transformToTree (l:ls) = case (words (replace ',' ' ' l)) of
    ["Leaf:", cls] -> ((Leaf cls):(transformToTree ls))
    ["Node:", intVal, fltVal] -> ((Node (read intVal) (read fltVal) EmptyTree EmptyTree):(transformToTree ls))
    _ -> error "Unexpected line found"

-- END OF Functions for the FIRST TASK

-- replace what -> by what -> where -> result
replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace w bw (x:xs)
    | w == x = (bw:(replace w bw xs))
    | otherwise = (x:(replace w bw xs))

-- Tree data type
-- The use of EmptyTree is essential for the algorithm used to Tree construction in inference tests
data Tree = EmptyTree
          | Leaf String 
          | Node Int Float Tree Tree
          deriving (Eq, Ord)

instance Show Tree where
    show tree = rmTrailingNewLine (showIndent tree 0) where
        showIndent :: Tree -> Int -> String
        showIndent EmptyTree _ = ""
        showIndent (Leaf cls) i = (replicate (2*i) ' ') ++ "Leaf: " ++ cls ++ "\n"
        showIndent (Node idx threshold lt rt) i = (replicate (2*i) ' ') ++ "Node: " ++ show idx ++ ", " ++ show threshold ++ "\n" ++ (showIndent lt (i+1)) ++ (showIndent rt (i+1))

        rmTrailingNewLine :: String -> String
        rmTrailingNewLine [] = []
        rmTrailingNewLine (x:[])
            | x == '\n' = []
            | otherwise = x:[]
        rmTrailingNewLine (x:xs) = x:(rmTrailingNewLine xs)