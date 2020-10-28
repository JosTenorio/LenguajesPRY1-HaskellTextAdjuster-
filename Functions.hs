module Functions where

import Data.List
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
insertBlanks amount pLine | (length pLine == 1) = pLine
                          | (amount >= length(pLine) - 1) = insertBlanks remainder (intercalate insertion [ [x] | x <- pLine] )
                          | otherwise = insertBlanksAux amount pLine
    where insertion = replicate (fromIntegral (amount) `quot` fromIntegral (length(pLine) - 1)) Blank
          remainder = (fromIntegral (amount) `mod` fromIntegral (length(pLine) - 1))
          insertBlanksAux amount' (x:xs) | (amount' == 0) = (x:xs)
                                         | (isBlank(x)) = x:insertBlanksAux amount' xs
                                         | otherwise = [x,Blank] ++ insertBlanksAux (amount'-1) xs



isBlank:: Token -> Bool
isBlank (Blank) = True
isBlank (_) = False

isWord:: Token -> Bool
isWord (Word _) = True
isWord (_) = False

isHypWord:: Token -> Bool
isHypWord (HypWord _) = True
isHypWord (_) = False
