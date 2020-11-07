{-          Module definition             -}
module Functions where


{-          Imports             -}
import Data.List (intersperse,  intercalate, isSuffixOf, (\\), dropWhileEnd )
import DataDef (HypMap, Token(HypWord), Line, Token(Blank), Token(Word))
import qualified Data.Map as Map
import Data.Char as Char (ord)

{-          Functions             -}

--Main Functions:

--a)
string2line :: String -> Line
string2line text =  [Word x | x <- words text]

--b)
line2string:: Line -> String
line2string pline =  init (line2stringAux (dropWhileEnd (== Blank) (dropWhile (== Blank) pline)))
    where line2stringAux [] = []
          line2stringAux (Word x:xs) = x ++ [' '] ++ line2stringAux (xs)
          line2stringAux (Blank :xs) =  [' '] ++ line2stringAux (xs)
          line2stringAux (HypWord x:xs) =  x ++ ['-',' '] ++ line2stringAux (xs)

--c)
tokenLength :: Token -> Int
tokenLength (Blank) = 1
tokenLength (Word x) = length (x)
tokenLength (HypWord x) = length (x) + 1 

--d)
lineLength :: Line -> Int
lineLength [] = 0
lineLength (x:[]) = tokenLength (x) 
lineLength (x:xs) = tokenLength (x) + 1 + lineLength (xs)

--e)
breakLine :: Int -> Line -> (Line,Line)
--Exceptions
breakLine _ [] = ([],[])
breakLine 1 x = ([],x)
breakLine max' remainder' = breakLineAux max' remainder' []
--Tail recursion
    where breakLineAux max remainder partition | (lineLength(partition ++ take 1 remainder) > max || remainder == []) = (partition,remainder) -- If it cant move more words, return result
                                               | otherwise   = breakLineAux max (tail remainder) (partition ++ [(head remainder)]) -- otherwise, move word, add to result, and keep recursion

--f)
mergers:: [String] -> [(String, String)]
--Exceptions 
mergers xs | (xs == []) = [("","")]
           | (length(xs) == 1) = [(head xs, "")]  
           | (length(xs) == 2) = [(head xs,  last xs)]
           | otherwise = mergersAux ([head xs], tail (xs)) []
--Tail recursion
mergersAux::([String],[String]) -> [(String, String)] -> [(String, String)]
mergersAux combination result | (length(snd(combination)) == 0) = result -- if all sub words have been moved, return result
--                            otherwise, move word to the left inside combination, add to result, and keep recursion
                              | otherwise = mergersAux ((fst combination) ++ [head(snd combination)], tail (snd combination)) (result ++ [(concat ((fst combination)), concat (snd combination))])

--g)
hyphenate :: HypMap -> Token -> [(Token,Token)]
--Exceptions 
hyphenate _ (Blank) = []
hyphenate _ (HypWord _) = []
--Identification of punctuation
hyphenate pMap' (Word x) | (isSuffixOf "..." x) = hyphenateAux pMap' x "..."
                        | (isSuffixOf "." x) = hyphenateAux pMap' x "."
                        | (isSuffixOf "," x) = hyphenateAux pMap' x ","
                        | (isSuffixOf ";" x) = hyphenateAux pMap' x ";"
                        | (isSuffixOf ":" x) = hyphenateAux pMap' x ":"
                        | (isSuffixOf "!" x) = hyphenateAux pMap' x "!"
                        | otherwise = hyphenateAux pMap' x ""
-- Helper function, if the word is not in the map return nothing
    where   hyphenateAux pMap word punctuation | (Map.notMember (word \\ punctuation) pMap) = []
                                               | otherwise = map (\y -> (HypWord (fst y) , Word (snd y ++ punctuation))) (mergers (pMap Map.! (word \\ punctuation)))
                                               -- otherwise, take all possible separations and convert them to HypWord-Word. Restores punctuation

--h)
lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
-- if the line requires mp separation, return it untouched
lineBreaks pMap limit pLine | (lineLength (pLine) <= limit) = [(pLine, [])]
                            | otherwise = [ y | y <- (fullLastWordPartition ++ [(init(pLine) ++ [fst x], [snd x]) | x <- lastWordPartitions]), lineLength (fst (y)) <= limit ]
    --                      otherwise, create array of hyphenated partitions + full separation partition, and filter that array with the limit for the first part of the partition
    where lastWordPartitions = (hyphenate pMap (last(pLine)))
          fullLastWordPartition = [(init(pLine), [last(pLine)])]

--i)
insertBlanks::Int-> Line -> Line
--Exceptions
insertBlanks _ ([]) = []
insertBlanks 0 pLine = pLine
-- 
insertBlanks amount pLine | (length pLine == 1) = pLine
--                        insert integer division blanks
                          | (amount >= length(pLine) - 1) = insertBlanks remainder (intercalate insertion [ [x] | x <- pLine] )
                          | otherwise = insertBlanksAux amount pLine
    where insertion = replicate (fromIntegral (amount) `quot` fromIntegral (length(pLine) - 1)) Blank -- integer division blanks
          remainder = (fromIntegral (amount) `mod` fromIntegral (length(pLine) - 1)) -- remainder of blanks after insertion blanks
          insertBlanksAux amount' (x:xs) | (amount' == 0) = (x:xs)                  -- Insertion of remainder, loops through list putting blanks after words
                                         | (isBlank(x)) = x:insertBlanksAux amount' xs
                                         | otherwise = [x,Blank] ++ insertBlanksAux (amount'-1) xs

isBlank:: Token -> Bool
isBlank (Blank) = True
isBlank (_) = False

--j)
separarYalinear::Int->String->String->HypMap->String->[String]
separarYalinear _  _ _ _ [] = []
separarYalinear limit pSeparar pAjustar pMap text  | ((pSeparar == "n") && (pAjustar == "n")) = separarYalinearAux1 limit textLine
                                                   | ((pSeparar == "n") && (pAjustar == "s")) = separarYalinearAux2 limit textLine
                                                   | ((pSeparar == "s") && (pAjustar == "n")) = separarYalinearAux3 limit textLine pMap 
                                                   | ((pSeparar == "s") && (pAjustar == "s")) = separarYalinearAux4 limit textLine pMap
                                                   | otherwise = [[]]
      where textLine = string2line text 


separarYalinearAux1::Int->Line->[String]
separarYalinearAux1 limit textLine  |((snd partitions) == []) = [(line2string (fst partitions))]
                                    |otherwise = (line2string (fst partitions)):(separarYalinearAux1 limit (snd partitions))
      where partitions = breakLine limit textLine


separarYalinearAux2::Int->Line->[String]
separarYalinearAux2 limit textLine  | ((snd partitions) == []) = [(line2string (fst partitions))]
                                    |otherwise = (line2string (insertBlanks filling (fst partitions))):(separarYalinearAux2 limit (snd partitions))
      where partitions = breakLine limit textLine
            filling = limit - (lineLength (fst partitions))

separarYalinearAux3::Int->Line->HypMap->[String]
separarYalinearAux3 limit textLine pMap | ((snd hyphenatedPartitions) == []) = [(line2string (fst hyphenatedPartitions))]
                                        |otherwise = (line2string (fst hyphenatedPartitions)):(separarYalinearAux3 limit (snd hyphenatedPartitions) pMap)
      where partitions = breakLine limit textLine
            hyphenation = last (lineBreaks pMap limit (fst partitions ++ (take 1 (snd partitions))))
            hyphenatedPartitions = (fst hyphenation, (snd hyphenation)  ++ (drop 1 (snd partitions))) 


separarYalinearAux4::Int->Line->HypMap->[String]
separarYalinearAux4 limit textLine pMap | ((snd hyphenatedPartitions) == []) = [(line2string (fst hyphenatedPartitions))]
                                        |otherwise = (line2string (insertBlanks filling(fst hyphenatedPartitions))):(separarYalinearAux4 limit (snd hyphenatedPartitions) pMap)
      where partitions = breakLine limit textLine
            hyphenation = last (lineBreaks pMap limit (fst partitions ++ (take 1 (snd partitions))))
            hyphenatedPartitions = (fst hyphenation, (snd hyphenation)  ++ (drop 1 (snd partitions)))
            filling = limit - (lineLength (fst hyphenatedPartitions))

--MainLoop Functions:
dict2String :: [(String, [String])] -> String
dict2String dict | (length dict == 0) = [] 
                 |otherwise = newLine ++ dict2String  (tail dict)
      where newLine = fst (head dict) ++ " " ++ concat (intersperse "-" (snd (head dict))) ++ "\n"

string2DictEntry::String -> (String, [String])
string2DictEntry line = ((head separated), divison )
      where
            separated = words line
            divison = words (fmap (\c -> if c=='-' then ' '; else c) (last separated))

split2string::[String] -> String
split2string split | (take 1 split  == []) = "Comando inválido" 
                   | otherwise = concat (intersperse "\n" split) 


normalizeText :: String -> String
normalizeText [] = []
normalizeText (x:xs)  | (Char.ord x == 9500 && Char.ord (head xs) == 237) = "á" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 9474) = "ó" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 161) = "í" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 9553) = "ú" ++ (normalizeText (drop 1 xs))
                      | (Char.ord x == 9500 && Char.ord (head xs) == 8976) = "é" ++ (normalizeText (drop 1 xs))
                      | otherwise = [x] ++ normalizeText xs

denormalizeText :: String -> String
denormalizeText [] = []
denormalizeText (x:xs) | (x == 'á') = "\9500\237" ++ denormalizeText xs
                       | (x == 'ó') = "\9500\9474" ++ denormalizeText xs
                       | (x == 'é') = "\9500\8976" ++ denormalizeText xs
                       | (x == 'ú') = "\9500\9553" ++ denormalizeText xs
                       | (x == 'í') = "\9500\161" ++ denormalizeText xs
                       | otherwise = [x] ++ denormalizeText xs