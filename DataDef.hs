module DataDef where

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)
