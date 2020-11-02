{-          Module definition             -}
module DataDef where

{-          Imports             -}
import qualified Data.Map as Map

{-          data definition             -}
type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

type HypMap = Map.Map String [String]