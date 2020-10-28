module DataDef where
import Data.Map ( Map )

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

type HypMap = Data.Map.Map String [String]