module Functions where

import Data.List ( intercalate, isSuffixOf, (\\), dropWhileEnd )
import DataDef (HypMap, Token(HypWord), Line, Token(Blank), Token(Word))
import qualified Data.Map as Map

string2line :: String -> Line
string2line text =  [Word x | x <- words text]


line2string:: Line -> String
line2string pline =  init (line2stringAux (dropWhileEnd (== Blank) (dropWhile (== Blank) pline)))
    where line2stringAux [] = []
          line2stringAux (Word x:xs) = x ++ [' '] ++ line2stringAux (xs)
          line2stringAux (Blank :xs) =  [' '] ++ line2stringAux (xs)
          line2stringAux (HypWord x:xs) =  x ++ ['-',' '] ++ line2stringAux (xs)

tokenLength :: Token -> Int
tokenLength (Blank) = 1
tokenLength (Word x) = length (x)
tokenLength (HypWord x) = length (x) + 1 


lineLength :: Line -> Int
lineLength [] = 0
lineLength (x:[]) = tokenLength (x) 
lineLength (x:xs) = tokenLength (x) + 1 + lineLength (xs)

breakLine :: Int -> Line -> (Line,Line)
breakLine _ [] = ([],[])
breakLine 1 x = ([],x)
breakLine max' remainder' = breakLineAux max' remainder' []
    where breakLineAux max remainder partition | (lineLength(partition ++ take 1 remainder) > max || remainder == []) = (partition,remainder)
                                               | otherwise   = breakLineAux max (tail remainder) (partition ++ [(head remainder)])



mergers:: [String] -> [(String, String)]
mergers xs | (xs == []) = [("","")]
           | (length(xs) == 1) = [(head xs, "")]  
           | (length(xs) == 2) = [(head xs,  last xs)]
           | otherwise = mergersAux ([head xs], tail (xs)) []

mergersAux::([String],[String]) -> [(String, String)] -> [(String, String)]
mergersAux combination result | (length(snd(combination)) == 0) = result
                              | otherwise = mergersAux ((fst combination) ++ [head(snd combination)], tail (snd combination)) (result ++ [(concat ((fst combination)), concat (snd combination))])

enHyp :: HypMap
enHyp = Map.fromList [ ("controla",["con","tro","la"]), 
                       ("futuro",["fu","tu","ro"]),
                       ("presente",["pre","sen","te"]),
                       ("maneja",["ma","ne","ja"]),
                       ("administrativo" , ["ad", "mi", "nis", "tra", "ti", "vo"])]

hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate _ (Blank) = []
hyphenate _ (HypWord _) = []
hyphenate pMap' (Word x) | (isSuffixOf "..." x) = hyphenateAux pMap' x "..."
                        | (isSuffixOf "." x) = hyphenateAux pMap' x "."
                        | (isSuffixOf "," x) = hyphenateAux pMap' x ","
                        | (isSuffixOf ";" x) = hyphenateAux pMap' x ";"
                        | (isSuffixOf ":" x) = hyphenateAux pMap' x ":"
                        | (isSuffixOf "!" x) = hyphenateAux pMap' x "!"
                        | otherwise = hyphenateAux pMap' x ""
    where   hyphenateAux pMap word punctuation | (Map.notMember (word \\ punctuation) pMap) = []
                                               | otherwise = map (\y -> (HypWord (fst y) , Word (snd y ++ punctuation))) (mergers (pMap Map.! (word \\ punctuation)))



lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaks pMap limit pLine | (lineLength (pLine) <= limit) = [(pLine, [])]
                            | otherwise = [ y | y <- (fullLastWordPartition ++ [(init(pLine) ++ [fst x], [snd x]) | x <- lastWordPartitions]), lineLength (fst (y)) <= limit ]
    where lastWordPartitions = (hyphenate pMap (last(pLine)))
          fullLastWordPartition = [(init(pLine), [last(pLine)])]



insertBlanks::Int-> Line -> Line
insertBlanks _ ([]) = []
insertBlanks 0 pLine = pLine 
insertBlanks amount pLine | (length pLine == 1) = pLine
                          | (amount >= length(pLine) - 1) = insertBlanks remainder (intercalate insertion [ [x] | x <- pLine] )
                          | otherwise = insertBlanksAux amount pLine
    where insertion = replicate (fromIntegral (amount) `quot` fromIntegral (length(pLine) - 1)) Blank
          remainder = (fromIntegral (amount) `mod` fromIntegral (length(pLine) - 1))
          insertBlanksAux amount' (x:xs) | (amount' == 0) = (x:xs)
                                         | (isBlank(x)) = x:insertBlanksAux amount' xs
                                         | otherwise = [x,Blank] ++ insertBlanksAux (amount'-1) xs


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

isBlank:: Token -> Bool
isBlank (Blank) = True
isBlank (_) = False
