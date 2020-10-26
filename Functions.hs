module Functions where

import DataDef (Token(Blank), Line)

string2line :: String -> Line
string2line [] = []
string2line (x:xs)| (x == ' ') =  Blank : string2line xs
                  | 
