-- -# LANGUAGE OverloadedStrings #-

-- Copyright China University of Water Resources and Electric Power (c) 2019-2020
-- All rights reserved.

module Corpus (
    POS,                -- String
    CateSymb,           -- String
    pos,                -- [POS]
    posCate,            -- [(POS, CateSymb)]
    phraStruList,       -- [PhraStru]
    LeftExtend,         -- [(Category,Tag,PhraStru)]
    LeftOver,           -- (Category,Tag,PhraStru)
    RightOver,          -- (Category,Tag,PhraStru)
    RightExtend,        -- [(Category,Tag,PhraStru)]
    OverType,           -- Int
    Prior(..),          -- Prior and its all Constructors
    StruGene,           -- (LeftExtend, LeftOver, RightOver, RightExtend, OverType, Prior)
    OverPair,           -- (PhraCate, PhraCate, Prior)
    getConn,            -- IO Connection
    psToCate,           -- IO ()
    rawToCate,          -- [(SqlValue]] -> [[SqlValue]]
    psToCateInASent,    -- String -> String
    getCateSymbFromPos, -- POS -> [(POS, CateSymb)] -> CateSymb
    copyCate,           -- IO ()
    copyPS,             -- IO ()
    resetStruGene_Id    -- IO ()
    
    ) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Data.List.Utils
import Category
import Phrase (Tag,PhraStru,Act,PhraCate)
import Utils

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
           ("aq","np/.np|(s\\.np)\\x(s\\.np)"),
           ("as","np/.np"),
           ("f","np/*np"),
           ("m","np/.np"),
           ("q","(np/.np)\\*(np/.np)|((s\\.np)/#(s\\.np))\\*(np/.np)|(s/*s)\\*(np/.np)"),
           ("r","np"),
           ("d","(s\\.np)/#(s\\.np)|(np/.np)/*(np/.np)"),
           ("p","((s\\.np)/#(s\\.np))/*np|((s\\.np)\\x(s\\.np))/*np|(s/*s)/*np"),
           ("c","(X\\*X)/*X|X\\*X|X/*X"),
           ("u","(np/*np)\\*np|(np/*np)\\*(np/.np)|(np/*np)\\*(s/.np)|((s\\.np)/#(s\\.np))\\*(np/.np)|((s\\.np)\\x(s\\.np))/*(np/.np)"),
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
phraStruList = ["MQ","XX","DHv","HvC","DHa","DHs","HaC","AHn","HnC","HmC","VO","OE","U1P","U2P","U3P","PO","SP","EM","DE","NR"]

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

-- Get a connection with given database.

getConn :: IO Connection
getConn =  connectMySQL defaultMySQLConnectInfo {
    mysqlHost = "127.0.0.1",
    mysqlUser = "hssc",
    mysqlPassword = "hssc",
    mysqlDatabase = "ccg4c"
    }

-- Data type for the string of SQL Select statement.
type SqlStat = String

-- To initialize the column cate_sent by the column raw_sent, translate each part of speech to its category.
psToCate :: IO ()
psToCate = do
    conn <- getConn
    stat1 <- prepare conn "select raw_sent2,serial_num from corpus"
    executeRaw stat1                                              --Get [[<raw_sent2>, <serial_num>]]
    rows <- fetchAllRows stat1
    putStrLn $ (show (length rows)) ++ " rows has been read."     --Select's result must be used.
    putStrLn $ "Maximal string length of cate_sent is " ++ (show $ maxStrLen $ map (fromSql.head) $ rawToCate rows)
 
--  colNames <- getColumnNames sth                   -- Get names of columns from results.
--  forM_ colNames $ \colName -> (putStrLn $ show colName)

    stat2 <- prepare conn "update corpus set cate_sent = ? where serial_num = ?"
    executeMany stat2 $ rawToCate rows               -- Update column cate_sent. 

    putStrLn "Category assignments are finished."

--  sth <- prepare conn "select serial_num,raw_sent,cate_sent from corpus"
--  executeRaw sth
--  rows <- fetchAllRows sth            -- Get [[<serial_num>, <cate_sent>]].
--  forM_ rows $ \row -> (forM_ row $ \fld -> putStrLn $ fromSql fld)
--  forM_ (last rows) $ \fld -> putStrLn $ fromSql fld
    commit conn
    disconnect conn             -- Explicitly close the connection.

-- Prepare [[<cate_sent>, <serial_num>]] from [[<raw_sent>,<serial_num>]]
rawToCate :: [[SqlValue]] -> [[SqlValue]]
rawToCate [] = [] 
rawToCate (row:rows) = ([toSql $ psToCateInASent $ fromSql $ head row]++[last row]):rawToCate rows

{- Translate <word>/<pos> into <word>:<cate> in a String, here <word> and <pos> are concrete word and its part of
   speech.
 -}

psToCateInASent :: String -> String
psToCateInASent [] = []
psToCateInASent xs = unwords $ map (\w -> (head $ split "/" w) ++ ":" ++ getCateSymbFromPos (last $ split "/" w) posCate) (words xs)
    
-- Get the category symbol from a part of speech, according to the list posCate.

getCateSymbFromPos :: POS -> [(POS, CateSymb)] -> CateSymb
getCateSymbFromPos [] _ = []
getCateSymbFromPos _ [] = []
getCateSymbFromPos pos (c:cs)      --Here, (c:cs) is just the list posCate
    | pos == fst c = snd c
    | otherwise = getCateSymbFromPos pos cs

{- Keep column cate_sent not changed, while column cate_sent2 modified manually. The initial values of column
   cate_sent2 are copied from column cate_sent. Actually cate_sent2 can be copied again from cate_sent where
   cate_check = 0. In other words, cate_check will be set 1 after cate_sent is checked by hand.
 -}

copyCate :: IO ()
copyCate = do
    conn <- getConn
    stat1 <- prepare conn "select cate_sent,serial_num from corpus where cate_check = 0"
    executeRaw stat1                                          --Get [[<cate_sent>, <serial_num>]]
    rows <- fetchAllRows stat1
    putStrLn $ (show $ length rows) ++ " rows has been read."

    stat2 <- prepare conn "update corpus set cate_sent2 = ? where serial_num = ?"
    executeMany stat2 rows                                    -- Update column cate_sent2 whose cate_check = 0. 
    commit conn
    disconnect conn                                           -- Close the connection.
 
{- Keep column raw_sent not changed, while column raw_sent2 modified manually. The initial values of column
   raw_sent2 are copied from column raw_sent. Actually raw_sent2 can be copied again from raw_sent where ps_check = 0.
   In other words, ps_check will be set 1 after raw_sent is checked by hand.
 -}

copyPS :: IO ()
copyPS = do
    conn <- getConn
    sth <- prepare conn "select raw_sent,serial_num from corpus where ps_check = 0"
    executeRaw sth
    rows <- fetchAllRows sth         --Get [[<raw_sent>, <serial_num>]]
                                     --Select's result must be used.
    putStrLn $ (show $ length rows) ++ " rows has been read."

    sth <- prepare conn "update corpus set raw_sent2 = ? where serial_num = ?"
    executeMany sth rows    -- Update column cate_sent2 whose ps_check = 0. 
    commit conn             -- Commit any pending data to the database.
    disconnect conn         -- Close the connection.

-- Again make Field 'id' in Table 'stru_gene' autoincrement from 1, used when 'id' values are not continuous.
resetStruGene_Id :: IO ()
resetStruGene_Id = do
    conn <- getConn
    stat1 <- prepare conn "select * from stru_gene where leftOver = ''"
    cols <- getColumnNames stat1
--  forM_ cols $ \col -> putStr (show col ++ " ")
    putStrLn "Field 'id' of Table 'stru_gene' has been reset."
    if (cols!!0) == "id"     -- Field 'id' exists.
      then do
        stat2 <- prepare conn "alter table stru_gene drop column id"
        executeRaw stat2
        stat3 <- prepare conn "alter table stru_gene add column id int(6) unsigned auto_increment primary key first"
        executeRaw stat3
      else do                -- Field 'id' doesn't exist.
        stat4 <- prepare conn "alter table stru_gene add column id int(6) unsigned auto_increment primary key first"
        executeRaw stat4
    disconnect conn

