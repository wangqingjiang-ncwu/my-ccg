{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power
-- All rights reserved.

module Corpus (
    POS,                 -- String
    CateSymb,            -- String
    pos,                 -- [POS]
    posCate,             -- [(POS, CateSymb)]
    phraStruList,        -- [PhraStru]
    LeftExtend,          -- [(Category,Tag,PhraStru)]
    LeftOver,            -- (Category,Tag,PhraStru)
    RightOver,           -- (Category,Tag,PhraStru)
    RightExtend,         -- [(Category,Tag,PhraStru)]
    OverType,            -- Int
    Prior(..),           -- Prior and its all Constructors
    StruGene,            -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    getRawSentForASent,  -- Int -> IO String
    getRawSent2ForASent, -- Int -> IO String
    getCateSentForASent, -- Int -> IO String
    getCateSent2ForASent,  -- Int -> IO String
    OverPair,            -- (PhraCate, PhraCate, Prior)
    posToCate,           -- IO ()
    posToCateForASent,   -- Int -> IO ()
    rawToCate,           -- [(MySQLValue]] -> [[MySQLValue]]
    rawToCateForASent,   -- MySQLValue -> MySQLValue
    posToCateInASent,    -- String -> String
    getCateSymbFromPos,  -- POS -> [(POS, CateSymb)] -> CateSymb
    copyCate,            -- IO ()
    copyCateForASent,    -- Int -> IO ()
    copyRawSent,         -- IO ()
    copyRawSentForASent, -- Int -> IO ()
    resetStruGene_Id,    -- IO ()
    setTreeScriptNull,   -- IO ()
    ClauIdx,             -- Int
    BanPCs,              -- [PhraCate]
    Script,              -- (ClauIdx,[[Rule]],BanPCs)
    Tree,                -- [PhraCate]
    Closure,             -- [PhraCate]
    Forest,              -- [[PhraCate]]
    readScripts,         -- String -> [Script]
    readScript,          -- String -> Script
    readPCList,          -- String -> [PhraCate]
    readTrees,           -- String -> [Tree]
    readRuleSet,         -- String -> [Rule]
    readRule,            -- String -> Rule
    readClosures,        -- String -> [Closure]
    readForests,         -- String -> [Forest]
    readForest,          -- String -> Forest
    scriptToString,      -- Script -> String
    nScriptToString,     -- [Script] -> String
    treeToString,        -- Tree -> String
    nTreeToString,       -- [Tree] -> String
    closureToString,     -- Closure -> String
    nClosureToString,    -- [Closure] -> String
    forestToString,      -- Forest -> String
    nForestToString      -- [Forest] -> String
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import Database.MySQL.Base
import Data.List.Utils
import Data.Tuple.Utils
import Category
import Phrase (Tag,PhraStru,Act,PhraCate,getPhraCateFromString,nPhraCateToString)
import Rule
import Utils
import Database

-- Datatype POS for parts of speech (word classes).
type POS = String

-- Datatype CateSymb for categorial symbols.
type CateSymb = String

-- Parts of speech (word classes) according to GBT20532-2006.
pos :: [POS]
pos = ["n","ng","nt","nd","nl","nh","ns","nn","ni","nz",
       "v","vt","vi","vl","vu","vd",
       "a","aq","as",
       "f",
       "m",
       "q",
       "r",
       "d",
       "p",
       "c",
       "u",                -- Auxiliary word #1, #2, and #3 are '的', '地', and '得' respectively.
       "e",
       "o",
       "i","in","iv","ia","ic",
       "j","jn","jv","ja",
       "h",
       "k",
       "g","gn","gv","ga",
       "x",
       "w","wp","ws","wu"]

-- Assignments from a part of speech to its syntactic category(ies).
posCate :: [(POS, CateSymb)]
posCate = [("n","np"),
           ("ng","np"),
           ("nt","np|(s\\.np)/#(s\\.np)|s/*s"),
           ("nd","np\\*np"),
           ("nl","np"),
           ("nh","np"),
           ("ns","np"),
           ("nn","np"),
           ("ni","np"),
           ("nz","np"),
           ("v","s\\.np|(s\\.np)/.np|((s\\.np)/.np)/.np"),
           ("vt","(s\\.np)/.np|((s\\.np)/.np)/.np"),
           ("vi","s\\.np"),
           ("vl","(s\\.np)/.np"),
           ("vu","(s\\.np)/#(s\\.np)"),
           ("vd","(s\\.np)\\x(s\\.np)"),
           ("a","np/.np"),
           ("aq","np/.np"),
           ("as","np/.np"),
           ("f","np/*np"),
           ("m","np/*np"),
           ("q","(np/*np)\\*(np/*np)|((s\\.np)/#(s\\.np))\\*(np/*np)"),
           ("r","np"),
           ("d","(s\\.np)/#(s\\.np)|(np/.np)/*(np/.np)"),
           ("p","((s\\.np)/#(s\\.np))/*np|((s\\.np)\\x(s\\.np))/*np|(s/*s)/*np"),
           ("c","(X\\*X)/*X|X\\*X|X/*X"),
           ("u","(np/*np)\\*np|((s\\.np)/#(s\\.np))\\*(np/.np)|((s\\.np)\\x(s\\.np))/*(np/.np)"),
           ("e","np|(s\\.np)/#(s\\.np)"),
           ("o","np|(s\\.np)/#(s\\.np)"),
           ("i","np|s\\.np|np/.np|s/*s"),
           ("in","np"),
           ("iv","s\\.np"),
           ("ia","np/.np"),
           ("ic","s/*s"),
           ("j","np|s\\.np|(s\\.np)/.np|np/.np"),
           ("jn","np"),
           ("jv","s\\.np|(s\\.np)/.np"),
           ("ja","np/.np"),
           ("h","np/*np"),
           ("k","np\\*np"),
           ("g","np|s\\.np|(s\\.np)/.np|np/.np"),
           ("gn","np"),
           ("gv","s\\.np|(s\\.np)/.np"),
           ("ga","np/.np"),
           ("x",""),
           ("w",""),
           ("wp","(X/*X)\\*X"),
           ("ws","np"),
           ("wu","")]

{- To now, the recognizable phrasal structures are as following.
   MQ: Quantity phrase
   XX: Conjunction phrase
   DHv: Adverbial-verb (headword) phrase
   HvC: Verb (headword)-complement phrase
   DHa: Adverbial-adjective (headword) phrase
   DHs: Adervbial-sentence phrase
   HaC: Adjective (headword)-complement phrase
   AHn: Attribute-noun (headword) phrase
   HnC: Noun (headword)-complement phrase
   HmC: Numeral (headword)-complement phrase
   VO: Verb-object phrase
   OE: Object extraction phrase
   U1P: 1-auxiliary word phrase, namely with '的' as end
   U2P: 2-auxiliary word phrase, namely with '地' as end
   U3P: 3-auxiliary word phrase, namely with '得' as end
   PO: Preposition object phrase
   SP: Subject-predicate phrase
   EM: Exclamation mood
   DE: Word, also considered as primitive phrase. "DE" means artificial designation.
   NR: Not recognizable phrase
   For future, there might be more kinds of phrases to be handled.
 -}

phraStruList :: [PhraStru]
phraStruList =  ["MQ","XX","DHv","HvC","DHa","DHs","HaC","AHn","HnC","HmC","VO","OE","U1P","U2P","U3P","PO","SP","EM","DE","NR"]

{- To indicate which phrasal structure is more prior in an overlapping pair, a left-adjacent phrase and a right-
   adjacent phrase should be considered. As basic fragments, such four phrasal structures would exist in many
   sentences, and act like human body genes.
   The structural gene is a 6-tuple (<leftExtend>, <leftOver>, <rightOver>, <rightExtend>, <overType>, <prior>), here
   <leftExtend> is a left adjacent phrase of <leftOver>, <rightExtend> is a right adjacent phrase of <rightOver>, and
   <leftOver> and <rightOver> are the left-to-right overlapping phrases, with <overType> to indicate overlapping type,
   and with <prior> to indicate which is prior to exist. Besides, <leftOver> and <rightOver> are at least one active,
   <leftExtend> and <leftOver> are at least one active, and <rightOver> and <rightExtend> are at least one active.
 -}

type LeftExtend = [(Category,Tag,PhraStru)]     -- Left neighbors
type LeftOver = (Category,Tag,PhraStru)         -- Overlapping left phrase
type RightOver = (Category,Tag,PhraStru)        -- Overlapping right phrase
type RightExtend = [(Category,Tag,PhraStru)]    -- Right neighbors
type OverType = Int                             -- Overlapping type
data Prior = Lp | Rp | Noth deriving (Eq, Read)    -- Lp means left prior, Rp means right prior, Noth means nothing.

instance Show Prior where
    show Lp = "Lp"
    show Rp = "Rp"
    show Noth = "Noth"

-- The structural genes are stored in table stru_gene of MySQL database ccg4c.

type StruGene = (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)

-- An overlapping pair of phrasal categories, including its priority assignment, used in clause parsing.

type OverPair = (PhraCate, PhraCate, Prior)

-- Get raw part-of-speech marked sentence indicated by serial_num.
getRawSentForASent :: Int -> IO String
getRawSentForASent sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select raw_sent from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    S.read is >>= \case
      Just [MySQLText v] -> return $ fromMySQLText (MySQLText v)
      Nothing -> return ""

-- Get revised part-of-speech marked sentence indicated by serial_num.
getRawSent2ForASent :: Int -> IO String
getRawSent2ForASent sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select raw_sent2 from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    S.read is >>= \case
      Just [MySQLText v] -> return $ fromMySQLText (MySQLText v)
      Nothing -> return ""

-- Get CCG syntactic types-marked sentence indicated by serial_num.
getCateSentForASent :: Int -> IO String
getCateSentForASent sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select cate_sent from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    S.read is >>= \case
      Just [MySQLText v] -> return $ fromMySQLText (MySQLText v)
      Nothing -> return ""

-- Get the revised CCG syntactic types-marked sentence indicated by serial_num.
getCateSent2ForASent :: Int -> IO String
getCateSent2ForASent sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select cate_sent2 from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    S.read is >>= \case
      Just [MySQLText v] -> return $ fromMySQLText (MySQLText v)
      Nothing -> return ""

-- To initialize the column cate_sent by the column raw_sent2, translate each part of speech to its category.
posToCate :: IO ()
posToCate = do
    conn <- getConn
    stmt <- prepareStmt conn "select raw_sent2,serial_num from corpus"
    (defs, is) <- queryStmt conn stmt []                          --([ColumnDef], InputStream [MySQLValue])
    rows <- S.toList is
    putStrLn $ (show (length rows)) ++ " rows has been read."     --Select's result must be consumed.
    let cate_sent_sns = rawToCate rows

--  putStrLn $ "Maximal length of cate_sent is " ++ (show $ maxStrLen $ map (fromMySQLText . head) cate_sent_sns)
--  forM_ defs $ \colName -> (putStrLn $ show colName)            -- Get names of columns from results.
    putStrLn $ show (cate_sent_sns!!0)
    putStrLn $ show (cate_sent_sns!!1)

    let stmt1 = "update corpus set cate_sent = ? where serial_num = ?"
    oks <- executeMany conn stmt1 cate_sent_sns                   -- Update column cate_sent.
--  oks <- executeMany conn stmt1 (take 3738 cate_sent_sns)       -- Restricted by Network.Socket.sendbuf
    putStrLn $ show (length oks) ++ " rows have been updated."

    closeStmt conn stmt
    close conn                                                    -- Explicitly close the connection.

-- Another version of posToCate used to initialize the column cate_sent for one sentence.
posToCateForASent :: Int -> IO ()
posToCateForASent sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select raw_sent2 from corpus where serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]            --([ColumnDef], InputStream [MySQLValue])
    raw_sent <- S.read is

    let cate_sent = case raw_sent of
                      Just x -> rawToCateForASent (head x)
                      Nothing -> error "posToCateForASent: No raw_sent was read."
--  putStrLn $ show raw_sent
--  putStrLn $ show cate_sent
    skipToEof is               -- Go to the end of the stream.

    stmt1 <- prepareStmt conn "update corpus set cate_sent = ? where serial_num = ?"
    ok <- executeStmt conn stmt1 [cate_sent, toMySQLInt32 sn]        -- Update column cate_sent.
    putStrLn $ "Row " ++ show sn ++ " has been updated."

    skipToEof is               -- Go to the end of the stream.
    closeStmt conn stmt
    closeStmt conn stmt1
    close conn          -- Explicitly close the connection.

-- Prepare [[<cate_sent>, <serial_num>]] from [[<raw_sent>,<serial_num>]]
rawToCate :: [[MySQLValue]] -> [[MySQLValue]]
rawToCate [] = []
rawToCate (row:rows) = ([toMySQLText $ posToCateInASent $ fromMySQLText $ head row]++[last row]):rawToCate rows

-- Another version of rawToCate for one sentence, preparing MySQLValue of <cate_sent> from that of <raw_sent>.
rawToCateForASent :: MySQLValue -> MySQLValue
rawToCateForASent (MySQLText "") = MySQLText ""
rawToCateForASent raw_sent = toMySQLText $ posToCateInASent $ fromMySQLText raw_sent

{- Translate <word>/<pos> into <word>:<cate> in a String, here <word> and <pos> are concrete word and its part of speech.
 -}

posToCateInASent :: String -> String
posToCateInASent [] = []
posToCateInASent xs = unwords $ map (\w -> (head $ split "/" w) ++ ":" ++ getCateSymbFromPos (last $ split "/" w) posCate) (words xs)

-- Get the category symbol from a part of speech, according to the list posCate.

getCateSymbFromPos :: POS -> [(POS, CateSymb)] -> CateSymb
getCateSymbFromPos [] _ = []
getCateSymbFromPos _ [] = []
getCateSymbFromPos pos (c:cs)      --Here, (c:cs) is just the list posCate
    | pos == fst c = snd c
    | otherwise = getCateSymbFromPos pos cs

{- Keep column cate_sent not changed, while column cate_sent2 modified manually. The initial values of column
   cate_sent2 are copied from column cate_sent. Actually cate_sent2 can be copied again from cate_sent where
   cate_check = 0. In other words, cate_check will be set 1 after cate_sent2 is checked by hand.
 -}
copyCate :: IO ()
copyCate = do
    conn <- getConn
    stmt <- prepareStmt conn "select cate_sent,serial_num from corpus where cate_check = 0"
    (def, is) <- queryStmt conn stmt []                                         --Get [[<cate_sent>, <serial_num>]]
    rows <- S.toList is
    putStrLn $ (show $ length rows) ++ " rows has been read."
    closeStmt conn stmt

    let stmt1 = "update corpus set cate_sent2 = ? where serial_num = ?"
    oks <- executeMany conn stmt1 rows                              -- Update column cate_sent2 whose cate_check = 0.
--  oks <- executeMany conn stmt1 (take 3500 rows)                  -- Restricted by Network.Socket.sendbuf
    putStrLn $ show (length oks) ++ " rows have been updated."
    close conn                                                      -- Close the connection.

{- Another version for copyCate, which complete copy column cate_sent to column cate_sent2 for given row. Actually
   cate_sent2 can be copied again from cate_sent when cate_check = 0. In other words, cate_check will be set 1 after cate_sent2 is checked by hand.
 -}
copyCateForASent :: Int -> IO ()
copyCateForASent sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select cate_sent from corpus where cate_check = 0 and serial_num = ?"
    (def, is) <- queryStmt conn stmt [toMySQLInt32 sn]          -- Get [[<cate_sent>]]
    cate_sent <- S.read is                                      -- Get Just [<cate_sent>]
    let cate_sent2 = case cate_sent of
                       Just x -> x!!0                           -- Get <cate_sent>
                       Nothing -> error "copyCateForASent: No cate_sent was read."
    skipToEof is        -- Go to the end of the stream, consuming result set before executing next SQL statement.

    stmt1 <- prepareStmt conn "update corpus set cate_sent2 = ? where serial_num = ?"
    ok <- executeStmt conn stmt1 [cate_sent2, toMySQLInt32 sn]        -- Update column cate_sent.
    putStrLn $ "Row " ++ show sn ++ " has been updated."

    closeStmt conn stmt
    closeStmt conn stmt1
    close conn

{- Keep column raw_sent not changed, while column raw_sent2 modified manually. The initial values of column
   raw_sent2 are copied from column raw_sent. Actually raw_sent2 can be copied again from raw_sent where ps_check = 0.
   In other words, pos_check will be set 1 after raw_sent2 is checked by hand.
 -}
copyRawSent :: IO ()
copyRawSent = do
    conn <- getConn
    stmt <- prepareStmt conn "select raw_sent,serial_num from corpus where pos_check = 0"
    (defs, is) <- queryStmt conn stmt []
    rows <- S.toList is              --Get [[MySQLText rs, MySQLInt32 sn]]
                                     --Select's result must be used.
    putStrLn $ (show $ length rows) ++ " rows have been read."
--  putStrLn $ show (rows!!0)

    let stmt1 = "update corpus set raw_sent2 = ? where serial_num = ?"
    oks <- executeMany conn stmt1 rows      -- Update column cate_sent2 whose ps_check = 0.
    putStrLn $ show (length oks) ++ " rows have been copied."

    closeStmt conn stmt
    close conn                       -- Close the connection.

{- Another version of copyRawSent for copying column raw_sent to column raw_sent2 for a given row. Actually raw_sent2 can be copied again from raw_sent where ps_check = 0. In other words, pos_check will be set 1 after raw_sent2 is checked by hand.
 -}
copyRawSentForASent :: Int -> IO ()
copyRawSentForASent sn = do
    conn <- getConn
    stmt <- prepareStmt conn "select raw_sent from corpus where pos_check = 0 and serial_num = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sn]
    raw_sent <- S.read is                -- Get Just [MySQLText]
    let raw_sent2 = case raw_sent of
                      Just x -> x!!0     -- Get MySQLText
                      Nothing -> error "copyRawSentForASent: No raw_sent was read."
    skipToEof is        -- Go to the end of the stream, consuming result set before executing next SQL statement.

    stmt1 <- prepareStmt conn "update corpus set raw_sent2 = ? where serial_num = ?"
    ok <- executeStmt conn stmt1 [raw_sent2, toMySQLInt32 sn]        -- Update column raw_sent.
    putStrLn $ "Row " ++ show sn ++ " has been updated."

    closeStmt conn stmt
    closeStmt conn stmt1
    close conn                          -- Close the connection.

-- Again make Field 'id' in Table 'stru_gene' autoincrement from 1, used when 'id' values are not continuous.
resetStruGene_Id :: IO ()
resetStruGene_Id = do
    conn <- getConn
    stmt <- prepareStmt conn "select * from stru_gene where leftOver = ''"
    (cols, is) <- queryStmt conn stmt []
    forM_ cols $ \col -> putStr (show col ++ " ")
    resetStmt conn stmt                            -- Reset a query statement

    putStrLn "Field 'id' of Table 'stru_gene' has been reset."
    if (show (getColumnName (cols!!0))) == "id"     -- Field 'id' exists.
      then do
        stmt <- prepareStmt conn "alter table stru_gene drop column id"
        executeStmt conn stmt []
        resetStmt conn stmt

        stmt <- prepareStmt conn "alter table stru_gene add column id int(6) unsigned auto_increment primary key first"
        executeStmt conn stmt []
      else do                -- Field 'id' doesn't exist.
        stmt <- prepareStmt conn "alter table stru_gene add column id int(6) unsigned auto_increment primary key first"
        executeStmt conn stmt []
    closeStmt conn stmt
    close conn

-- Initialize Fields 'tree' and 'script' in Table 'corpus' as value "[]".
setTreeScriptNull :: IO ()
setTreeScriptNull = do
    conn <- getConn
    stmt <- prepareStmt conn "update corpus set tree = '[]' where isnull(tree)"
    executeStmt conn stmt []
    resetStmt conn stmt

    stmt <- prepareStmt conn "update corpus set script = '[]' where isnull(script)"
    executeStmt conn stmt []
    closeStmt conn stmt
    close conn

{- A script is a triple, recording parsing instructions for a clause, which include the serial number of the clause,
   category-converted rules for every trip of recursive parsing, and all banned phrasal categories.
 -}
type ClauIdx = Int
type BanPCs = [PhraCate]
type Script = (ClauIdx, [[Rule]], BanPCs)

-- A Tree is actually a list of PhraCate.
type Tree = [PhraCate]

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

-- Read [[PhraCate]] from a String.
readTrees :: String -> [[PhraCate]]
readTrees str = map readPCList (stringToList str)

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
    | str == "Hn/a" = Hna
    | str == "P/a" = Pa
    | str == "Cv/a" = Cva
    | str == "Cn/a" = Cna
    | str == "A/n" = An
    | otherwise = error "readRule: Input string is not recognized."

scriptToString :: Script -> String
scriptToString script = "(" ++ clauIdx ++ "," ++ ruleSets ++ "," ++ banPCs ++ ")"
    where
      clauIdx = show (fst3 script)
      ruleSets = show (snd3 script)
      banPCs = nPhraCateToString (thd3 script)

-- Get the String from a [Script] value.
nScriptToString :: [Script] -> String
nScriptToString scripts = listToString (map scriptToString scripts)

-- Get the String from a Tree value.
treeToString :: Tree -> String
treeToString tree = nPhraCateToString tree

-- Get the String fron a [Tree] value.
nTreeToString :: [Tree] -> String
nTreeToString trees = listToString (map treeToString trees)

type Closure = [PhraCate]
type Forest = [[PhraCate]]

-- Read [Closure] from a String.
readClosures :: String -> [Closure]
readClosures str = map readPCList (stringToList str)

-- Read [Forest] from a String.
readForests :: String -> [Forest]
readForests str = map readForest (stringToList str)

-- Read Forest from a String
readForest :: String -> Forest
readForest str = map readPCList (stringToList str)

-- Get the String from a Closure value.
closureToString :: Closure -> String
closureToString closure = nPhraCateToString closure

-- Get the String from a [Closure] value.
nClosureToString :: [Closure] -> String
nClosureToString nClo = listToString (map closureToString nClo)

-- Get the String from a Forest value.
forestToString :: Forest -> String
forestToString forest = listToString (map nPhraCateToString forest)

-- Get the String from a [Forest] value.
nForestToString :: [Forest] -> String
nForestToString nForest = listToString (map forestToString nForest)
