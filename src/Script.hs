-- Copyright (c) 2019-2020 China University of Water Resources and Electric Power, 
-- All rights reserved.

module Script (
    ClauIdx,              -- Int
    BanPCs,               -- [PhraCate]
    Script,               -- [(ClauIdx,[[Rule]],BanPCs)]
    readScripts,          -- String -> [Script]
    readScript,           -- String -> Script
    readPCList,           -- String -> [PhraCate]
    readRuleSet,          -- String -> [Rule]
    readRule,             -- String -> Rule
    scriptToString,       -- Script -> String
    nScriptToString       -- [Script] -> String
    ) where

import Category
import Phrase
import Parse
import Utils
import Data.Tuple.Utils

{- A script is a triple, recording parsing instructions for a clause, which include the serial number of the clause,
   category-converted rules for every trip of recursive parsing, and all banned phrasal categories.
 -}
type ClauIdx = Int
type BanPCs = [PhraCate]
type Script = (ClauIdx, [[Rule]], BanPCs)

-- Read Scripts from a String.
readScripts :: String -> [Script]
readScripts str = map readScript (stringToList str)

-- Read a Script from a String.
readScript :: String -> Script
readScript str = (cid, ruleSets, banPCs)
    where
      str' = stringToTriple str
      cid = read (fst3 str') :: Int
      ruleSets = map readRuleSet (stringToList (snd3 str'))
      banPCs = readPCList (thd3 str')

-- Read [PhraCate] from a String.
readPCList :: String -> [PhraCate]
readPCList str = map getPhraCateFromString (stringToList str)

-- Read a rule set from the String of this rule set.
readRuleSet :: String -> [Rule]
readRuleSet str = map readRule (stringToList str)

-- Read a rule from a string.
readRule :: String -> Rule
readRule str
    | str == "S/s" = Ss
    | str == "O/s" = Os
    | str == "A/s" = As
    | str == "S/v" = Sv
    | str == "O/v" = Ov
    | str == "A/v" = Av
    | str == "Hn/v" = Hnv
    | str == "D/v" = Dv
    | str == "S/a" = Sa
    | str == "O/a" = Oa
    | str == "P/a" = Pa
    | str == "Cv/a" = Cva
    | str == "Cn/a" = Cna
    | str == "A/n" = An
    | otherwise = error "readRule: Input string is not recognized."

-- Get the String from a Script value.
scriptToString :: Script -> String
scriptToString script = "(" ++ clauIdx ++ "," ++ ruleSets ++ "," ++ banPCs ++ ")"
    where
      clauIdx = show (fst3 script)
      ruleSets = show (snd3 script)
      banPCs = nPhraCateToString (thd3 script)
     
-- Get the String from a [Script] value.
nScriptToString :: [Script] -> String
nScriptToString scripts = listToString (map scriptToString scripts)

