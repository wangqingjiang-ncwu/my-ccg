{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Copyright (c) 2019-2023 China University of Water Resources and Electric Power
-- All rights reserved.

module Corpus (
    POS,                 -- String
    CateSymb,            -- String
    pos,                 -- [POS]
    posCate,             -- [(POS, CateSymb)]
    phraStruList,        -- [PhraStru]
    getRawSentForASent,  -- Int -> IO String
    getRawSent2ForASent, -- Int -> IO String
    getCateSentForASent, -- Int -> IO String
    getCateSent2ForASent,  -- Int -> IO String
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
    Tree,                -- (ClauIdx, [PhraCate])
    quickSortForTree,    -- [Tree] -> [Tree]
    quickSortForScript,  -- [Script] -> [Script]
    Closure,             -- [PhraCate]
    Forest,              -- [[PhraCate]]
    readScripts,         -- String -> [Script]
    readScript,          -- String -> Script
    readPCList,          -- String -> [PhraCate]
    readTree,            -- String -> Tree
    readTrees,           -- String -> [Tree]
    readSLROfSent,       -- String -> SLROfSent
    getTreeDepth,        -- [PhraCate] -> Int
    readRuleSet,         -- String -> [Rule]
    readRule,            -- String -> Rule
    readClosures,        -- String -> [Closure]
    readForests,         -- String -> [Forest]
    readForest,          -- String -> Forest
    scriptToString,      -- Script -> String
    nScriptToString,     -- [Script] -> String
    treeToString,        -- Tree -> String
    nTreeToString,       -- [Tree] -> String
    aSLRToString,        -- SLROfATrans -> String
    clauseSLRToString,   -- SLROfClause -> String
    nClauseSLRToString,   -- SLROfSent -> String
    closureToString,     -- Closure -> String
    nClosureToString,    -- [Closure] -> String
    forestToString,      -- Forest -> String
    nForestToString,     -- [Forest] -> String
    setIpcOfRows,        -- Int -> Int -> String -> IO ()
    removeLineFeed,      -- IO ()
    removeLineFeedForOneRow,    -- [MySQLValue, MySQLValue, MySQLValue] -> [MySQLValue, MySQLValue, MySQLValue]
    addClauIdxToTreeField       -- Int -> Int -> IO ()
    ) where

import Control.Monad
import qualified System.IO.Streams as S
import qualified Data.String as DS
import Database.MySQL.Base
import Data.List.Utils
import Data.List
import Data.Tuple.Utils
import Category
import Phrase (Tag,stOfCate,spOfCate,ssOfCate,getPhraBySpan,PhraStru,Act,PhraCate,getPhraCateFromString,nPhraCateToString)
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
       "v","vt","vt2","vi","vl","vu","vd",                                      -- vt2 is for double objects.
       "a","aq","as",
       "f",
       "m",
       "q",
       "r",
       "d",
       "dv","da","ds",                 -- dv, da and ds are adverbs respectively modify verbs, adjectives and sentences.
       "p",
       "pa","pb",                      -- '把' and '被'
       "c",                            -- Bidirectional conjunctions
       "cb","cf",                      -- Backward and forward conjunctions
       "u",                            -- Auxiliary word #1, #2, #3, #4, #5, and #6 are '的', '地', '得', '着|了|过', '等|似的', and '所' respectively.
       "u1","u2","u3","u4","u5","u6",
       "y",                                                                     -- 语气词，吗、呢、了、...
       "e",
       "o",
       "i","in","iv","ia","ic",
       "j","jn","jv","jvi","jvt","ja",
       "h",
       "k",
       "g","gn","gv","gvi","gvt","ga",
       "x",
       "w","wp","ws","wu"]

-- Assignments from a part of speech to its syntactic category(ies).
posCate :: [(POS, CateSymb)]
posCate = [("n","np"),
           ("ng","np"),
           ("nt","(s\\.np)/#(s\\.np)"),                                         -- Using N/d and Ds/d to get np and s/*s.
           ("nd","np\\*np"),
           ("nl","np"),
           ("nh","np"),
           ("ns","np"),
           ("nn","np"),
           ("ni","np"),
           ("nz","np"),
           ("v","s\\.np|(s\\.np)/.np|((s\\.np)/.np)/.np"),
           ("vt","(s\\.np)/.np"),                                               -- For only one object.
           ("vt2","((s\\.np)/.np)/.np"),                                        -- For indirected object and directed object.
           ("vi","s\\.np"),
           ("vl","(s\\.np)/.np"),
           ("vu","(s\\.np)/#(s\\.np)"),
           ("vd","(s\\.np)\\x(s\\.np)"),
           ("a","np/.np"),
           ("aq","np/.np"),
           ("as","np/.np"),
           ("f","np/*np"),
           ("m","np/*np"),
           ("q","(np/*np)\\*(np/*np)"),                -- 数量短语做定语的频次最高
           ("r","np"),
           ("d","(s\\.np)/#(s\\.np)"),                 -- 副词做动词状语的频次最高，去掉(np/.np)/*(np/.np)
           ("dv","(s\\.np)/#(s\\.np)"),                -- 动词的状语，未用
           ("da","(np/.np)/*(np/.np)"),                -- 形容词的状语
           ("dx","(s\\.np)/x(s\\.np)"),                -- 趋向动词的状语
           ("ds","s/*s"),                              -- 句子的状语
           ("p","((s\\.np)/#(s\\.np))/*np"),           -- 通过Cv/d、Ds/d，去掉了类型((s\\.np)\\x(s\\.np))/*np、(s/*s)/*np"
           ("pa","((s/.np)\\.np)/#((s\\.np)/.np)"),    -- 介词'把'的类型，宾语提前到动语前
           ("pb","(s/#(s/.np))\\.np"),                 -- 介宾'被'的类型，宾语提取到主语前
           ("c","(X\\*X)/*X"),                         -- 连词的典型类型，双向连词，分别通过Cb/c、Cf/c得到后向、前向连词。
           ("cb","X\\*X"),                             -- 后向连词
           ("cf","X/*X"),                              -- 前向连词
           ("u","(np/*np)\\*np|((s\\.np)/#(s\\.np))\\*(np/.np)|((s\\.np)\\x(s\\.np))/*(np/.np)|((np/.np)\\*(np/.np))/*((np/.np)/*(np/.np))|(s\\.np)\\x(s\\.np)|(np/.np)\\*(np/.np)|X\\*X"),
           ("u1","(np/*np)\\*np"),                     -- 的
           ("u2","((s\\.np)/#(s\\.np))\\*(np/.np)"),   -- 地
           ("u3","((s\\.np)\\x(s\\.np))/*(np/.np)"),   -- 得，通过U3d/u3，去掉了类型((np/.np)\\*(np/.np))/*((np/.np)/*(np/.np))
           ("u4","(s\\.np)\\x(s\\.np)"),               -- 着、了、过
           ("u5","X\\#X"),                             -- 等、似的
           ("u6","np/*((s\\.np)/.np)"),                -- 所
           ("y","X\\.X"),                              -- 语气词，吗、呢、了、...
           ("e","np|(s\\.np)/#(s\\.np)"),
           ("o","np|(s\\.np)/#(s\\.np)"),
           ("i","np|s\\.np|np/.np|s/*s"),
           ("in","np"),
           ("iv","s\\.np"),
           ("ia","np/.np"),
           ("id","(s\\.np)/#(s\\.np)"),
           ("ic","s/*s"),
           ("j","np|s\\.np|(s\\.np)/.np|np/.np"),
           ("jn","np"),
           ("jv","s\\.np|(s\\.np)/.np"),
           ("jvi","s\\.np"),
           ("jvt","(s\\.np)/.np"),
           ("ja","np/.np"),
           ("h","np/*np"),                            -- 阿、老、初、第
           ("k","np\\*X"),                            -- 子、儿、（工作）者、（我）们、（新）式、（四年）制、
           ("g","np|s\\.np|(s\\.np)/.np|np/.np"),
           ("gn","np"),
           ("gv","s\\.np|(s\\.np)/.np"),
           ("gvi","s\\.np"),
           ("gvt","(s\\.np)/.np"),
           ("ga","np/.np"),
           ("x",""),
           ("w",""),
           ("wc","(X\\*X)/*X"),                       -- 顿号（、），有时的逗号（，）
           ("wn","np"),
           ("ws","np"),                               -- 外文字符串
           ("wu","")]

{- To now, the recognizable phrasal structures are as following.
   MQ: Numeral Quantifier phrase
   PQ: Pronoun Quantifier phrase, such as "这r 个q", "这r 筐q"
   XX: Conjunction phrase
   CC: Clause Coordination
   DHv: Adverbial-verb (headword) phrase
   HvC: Verb (headword)-complement phrase
   DHa: Adverbial-adjective (headword) phrase
   DHs: Adervbial-sentence phrase
   DHd: Adverbial-adverb (headword) phrase
   DHx: Adverbial-directioanl verb phrase
   DHoe: Adverbial-object extraction phrase, such as "最近d 他说oe 的 话"
   DHas: Adverbial-ba structure phrase, such as "甚至d 把他as 连上三级"
   HaC: Adjective (headword)-complement phrase
   AHn: Attribute-noun (headword) phrase
   HnC: Noun (headword)-complement phrase
   HmC: Numeral (headword)-complement phrase
   VO: Verb-object phrase
   OE: Object extraction phrase
   PE: Predicate extraction phrase, such as "我在力所能及的范围内，做好自己的工作。"
   U1P: 1-auxiliary word phrase, namely with '的' as end
   U2P: 2-auxiliary word phrase, namely with '地' as end
   U3P: 3-auxiliary word phrase, namely with '得' as end
   U3Pv: U3P phrase for verb complement, such as "干 得 好"
   U3Pa: U3P phrase for adjective complement, such as "好 得 很"
   U4P: 4-auxiliary word phrase, namely with '着|了|过' as end, identical to HvC.
   U5P: 5-auxiliary word phrase, namely with '等|似的|一样' as end
   U6P: 6-auxiliary word phrase, namely with '所' as head
   PO: Preposition object phrase
   MOv: Move object before verb, namely '把'字结构, which is one kind of PO.
   MOs: Move object before subject, namely '被'字结构 which is not same as traditional '被'字结构, and is also one kind of PO.
   SP: Subject-predicate phrase
   TP: Tone Phrase
   EM: Exclamation mood
   HP: Prefix phrase, such as "老 三", "第 一", and "初 一".
   KP: Postfix phrase, such as "工作 者", "我 们", and "中 式".
   DE: Word, also considered as primitive phrase. "DE" means artificial designation.
   NR: Not recognizable phrase
   For future, there might be more kinds of phrases to be handled.
 -}

phraStruList :: [PhraStru]
phraStruList =  ["MQ","PQ","XX","CC","DHv","HvC","DHa","DHs","DHd","DHx","DHoe","HaC","AHn","HnC","HmC","VO","OE","PE","U1P","U2P","U3P","U3Pv","U3Pa","U4P","U5P","U6P","PO","MOv","MOs","SP","TP","EM","HP","KP","DE","NR"]

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
--    putStrLn $ show raw_sent

    let cate_sent = case raw_sent of
                      Just x -> rawToCateForASent (head x)
                      Nothing -> error "posToCateForASent: No raw_sent was read."

    putStrLn $ show cate_sent
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

-- A Tree is a clausal serial number and a list of PhraCate.
type Tree = (ClauIdx, [PhraCate])

-- Quick sort for [Tree] according their ClauIdx field, where Tree :: (ClauIdx, [PhraCate]).
quickSortForTree :: [Tree] -> [Tree]
quickSortForTree [] = []
quickSortForTree (t:ts) = (quickSortForTree [x|x<-ts, fst x < fst t]) ++ [t] ++ (quickSortForTree [x|x<-ts, fst x >= fst t])

-- Quick sort for [Script] according ClauIdx field, where Script :: (ClauIdx,[[Rule]],BanPCs).
quickSortForScript :: [Script] -> [Script]
quickSortForScript [] = []
quickSortForScript (s:ss) = (quickSortForScript [x|x<-ss, fst3 x < fst3 s]) ++ [s] ++ (quickSortForScript [x|x<-ss, fst3 x >= fst3 s])

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

-- Read (ClauIdx, [PhraCate]) from a String.
readTree :: String -> Tree
readTree str = (clauIdx, phraCateList)
    where
    (clauIdxStr, phraCateListStr) = stringToTuple str
    clauIdx = read clauIdxStr :: Int
    phraCateList = map getPhraCateFromString (stringToList phraCateListStr)

-- Read [Tree] from a String.
readTrees :: String -> [Tree]
readTrees str = map readTree (stringToList str)

readSLROfTrans :: String -> SLROfATrans
readSLROfTrans str = (stub, rule)
    where
      tupleStr = stringToTuple str
      stub = map getPhraCateFromString $ stringToList (fst tupleStr)
      rule = readRuleSet (snd tupleStr)

readSLROfClause :: String -> SLROfClause
readSLROfClause str = map readSLROfTrans (stringToList str)

readSLROfSent :: String -> SLROfSent
readSLROfSent str = map readSLROfClause (stringToList str)

-- Get the depth of a tree, namely the biggest depth of its leaves.
getTreeDepth :: [PhraCate] -> Int                          -- Here a tree is only considered its phrasal categories.
getTreeDepth [] = 0                                        -- Empty tree
getTreeDepth [r] = 1                                       -- Only root
getTreeDepth t = if (roots == [] || length roots > 1)
                   then (-1)                               -- No parsing tree or more than one parsing tree.
                   else (1 + maximum [getTreeDepth lt, getTreeDepth rt])      -- ClauIdx 0 is nonsense.
    where
      leafNum = length (getPhraBySpan 0 t)                       -- Phrase length
      roots = getPhraBySpan (leafNum - 1) t
      secStart = ssOfCate (roots!!0)                             -- Suppose only one root
      lrt = [x | x <- t, spOfCate x /= leafNum - 1]              -- Remove the root
      lt = [x | x <- lrt, stOfCate x < secStart]                 -- Left subtree
      rt = [x | x <- lrt, stOfCate x >= secStart]                -- Right subtree

-- Read a rule set from the String of this rule set.
readRuleSet :: String -> [Rule]
readRuleSet str = map readRule (stringToList str)

-- Read a rule from a string.
readRule :: String -> Rule
readRule str
    | str == "S/s" = Ss        -- s1
    | str == "P/s" = Ps        -- s2
    | str == "O/s" = Os        -- s3
    | str == "A/s" = As        -- s4
    | str == "Hn/s" = Hns      -- s5
    | str == "N/s" = Ns        -- s5
    | str == "S/v" = Sv        -- v1
    | str == "O/v" = Ov        -- v2
    | str == "A/v" = Av        -- v3
    | str == "Hn/v" = Hnv      -- v4
    | str == "D/v" = Dv        -- v5
    | str == "Cn/v" = Cnv      -- v6
    | str == "Cv/v" = Cvv      -- v7
    | str == "N/v" = Nv        -- v8
    | str == "P/vt" = Pvt      -- v9
    | str == "OE/vt" = OEvt    -- v10
    | str == "Vt/vi" = Vtvi    -- v11
    | str == "A/vd" = Avd      -- v12
    | str == "S/a" = Sa        -- a1
    | str == "P/a" = Pa        -- a2
    | str == "V/a" = Va        -- a3
    | str == "O/a" = Oa        -- a4
    | str == "D/a" = Da        -- a5
    | str == "Da/a" = Daa      -- a6
    | str == "Ca/a" = Caa      -- a7
    | str == "Cn/a" = Cna      -- a8
    | str == "Cv/a" = Cva      -- a9
    | str == "Hn/a" = Hna      -- a10
    | str == "N/a" = Na        -- a11
    | str == "P/n" = Pn        -- n1
    | str == "V/n" = Vn        -- n2
    | str == "A/n" = An        -- n3
    | str == "Cn/n" = Cnn      -- n4
    | str == "Cv/n" = Cvn      -- n5
    | str == "D/n" = Dn        -- n6
    | str == "Da/n" = Dan      -- n7
    | str == "ADJ/n" = ADJn    -- n8
    | str == "S/nd" = Snd      -- n9
    | str == "O/nd" = Ond      -- n10
    | str == "Hn/nd" = Hnnd    -- n11
    | str == "S/d" = Sd        -- d1
    | str == "O/d" = Od        -- d2
    | str == "A/d" = Ad        -- d3
    | str == "Hn/d" = Hnd      -- d4
    | str == "Cv/d" = Cvd      -- d5
    | str == "N/d" = Nd        -- d6
    | str == "ADJ/d" = ADJd    -- d7
    | str == "Da/d" = Dad      -- d8
    | str == "Ds/d" = Dsd      -- d9
    | str == "Dx/d" = Dxd      -- d10
    | str == "Doe/d" = Doed    -- d11
    | str == "D/p" = Dp        -- p1
    | str == "O/oe" = Ooe      -- oe1
    | str == "Hn/oe" = Hnoe     -- oe2
    | str == "N/oe" = Noe      -- oe3
    | str == "N/pe" = Npe      -- pe1
    | str == "A/q" = Aq        -- q1
    | str == "Jf/c" = Jfc      -- c1
    | str == "Jb/c" = Jbc      -- c2
    | str == "U3d/u3" = U3du3  -- au1
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
treeToString tree = "(" ++ show (fst tree) ++ "," ++ nPhraCateToString (snd tree) ++ ")"

-- Get the String from a [Tree] value.
nTreeToString :: [Tree] -> String
nTreeToString trees = listToString (map treeToString trees)

type Stub = [PhraCate]
type SLROfATrans = (Stub, [Rule])
type SLROfClause = [SLROfATrans]
type SLROfSent = [SLROfClause]

aSLRToString :: SLROfATrans -> String
aSLRToString slr = "(" ++ stub ++ "," ++ ruleSets ++ ")"
    where
      stub = nPhraCateToString (fst slr)
      ruleSets = show (snd slr)

clauseSLRToString :: SLROfClause -> String
clauseSLRToString clauSLR = listToString (map aSLRToString clauSLR)

nClauseSLRToString :: SLROfSent -> String
nClauseSLRToString clauSLRs = listToString (map clauseSLRToString clauSLRs)

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

{- Set attribue 'ipc' of certain rows as an user of this software. In table corpus,
   column 'ipc' records the intellectual property creator (IPC), who will complete or have
   completed the parsing of one row (namely sentence).
   ONLY user 'wqj' can execute this function to designate a certain software user as IPC of certain rows,
   but this access control has not implemented.
 -}
setIpcOfRows :: Int -> Int -> String -> IO ()
setIpcOfRows startRow endRow username = do
    conn <- getConnByUserWqj
    stmt <- prepareStmt conn "select * from user where name = ?"
    (defs, is) <- queryStmt conn stmt [toMySQLText username]
    row <- S.read is                -- Get Just [MySQLText]
    let username' = case row of
                      Just x -> fromMySQLText (x!!0)     -- The first element of x is MySQLText 'name'
                      Nothing -> error "setIpcOfRows: No this user."
    skipToEof is        -- Go to the end of the stream, consuming result set before executing next SQL statement.
    closeStmt conn stmt
    if (username' /= username)
      then error "setIpcOfRows: Unexpected failure."
      else do
        stmt1 <- prepareStmt conn "update corpus set ipc = ? where serial_num >= ? && serial_num <= ?"
        ok <- executeStmt conn stmt1 [toMySQLText username, toMySQLInt32 startRow, toMySQLInt32 endRow]   -- Update column ipc.
        putStrLn $ show (getOkAffectedRows ok) ++ " rows have been updated."      -- Only rows with their values changed are affected rows.
        closeStmt conn stmt1
    close conn                       -- Close the connection.

{- There might be line feed character '\r', '\n', or "\r\n" in field raw_sent and raw_sent2, which makes the csv file
   exported from table corpus can't be used directly for importing. The function removeLineFeed removes these line-
   terminated characters.
   ONLY user 'wqj' can execute this function to designate a certain software user as IPC of certain rows,
   but this access control has not implemented.
  -}
removeLineFeed :: IO ()
removeLineFeed = do
    conn <- getConnByUserWqj
    stmt <- prepareStmt conn "select raw_sent, raw_sent2, serial_num from corpus"
    (defs, is) <- queryStmt conn stmt []
    rows <- S.toList is
    let rows' = map removeLineFeedForOneRow rows
    let stmt1 = "update corpus set raw_sent = ?, raw_sent2 = ? where serial_num = ?"
    oks <- executeMany conn stmt1 rows'
    putStrLn $ show (length oks) ++ " rows have been updated."      -- Only rows with their values changed are affected rows.
    closeStmt conn stmt
    close conn                       -- Close the connection.

{- Remove line-terminated characters '\r', '\n', or "\r\n" from MySQL values.
 - The input is raw_sent, raw_sent2, and serial_num for one row in Table corpus.
 -}
removeLineFeedForOneRow :: [MySQLValue] -> [MySQLValue]
removeLineFeedForOneRow vs = [rs', rs2', vs!!2]
    where
    rsStr = fromMySQLText (vs!!0)                      -- The field raw_sent
    rs2Str = fromMySQLText (vs!!1)                     -- The field raw_sent2
    rsStrNoLF = replace "\n" "" $ replace "\r" "" rsStr
    rs2StrNoLF = replace "\n" "" $ replace "\r" "" rs2Str
    rs' = toMySQLText rsStrNoLF
    rs2' = toMySQLText rs2StrNoLF

{- Add clause index to field tree in treebank. Treebank is selected by property 'tree_target' in configuration file.
 - The serial number range of sentences are designated by input parameter 'sentSnOfStart' and 'sentSnOfEnd'.
 - Clause indices make removing and paring again desiganated clauses feasible.
 - This function should be run under ghci.
 -}
addClauIdxToTreeField :: Int -> Int -> IO ()
addClauIdxToTreeField sentSnOfStart sentSnOfEnd = do
    confInfo <- readFile "Configuration"                                        -- Read the local configuration file
    let tree_target = getConfProperty "tree_target" confInfo

    conn <- getConn
    let sqlstat = DS.fromString $ "select tree, serial_num from " ++ tree_target ++ " where serial_num >= ? && serial_num <= ?"
    stmt <- prepareStmt conn sqlstat
    (defs, is) <- queryStmt conn stmt [toMySQLInt32 sentSnOfStart, toMySQLInt32 sentSnOfEnd]           -- read rows whose serial_nums are in designated range.
    rows <- S.toList is

    if (rows == [])
      then
        putStrLn "addClauIdxToTreeField: No sentence is asked to do this operation."
      else do
        let bracketSnList = map (\row -> (((!!1) . fromMySQLText . (!!0)) row, (fromMySQLInt32 . (!!1)) row)) $ filter (\row -> (fromMySQLText . (!!0)) row /= "[]") rows 
                                             -- [(Char, Int)], the 2nd char i.e. bracket and serial_num
                                             -- The rows with 'tree' value "[]" are filtered out.
        putStrLn $ "addClauIdxToTreeField: bracketSnList: " ++ show bracketSnList
        prevRes <- someFinished False bracketSnList
        if prevRes
          then putStrLn "addClauIdxToTreeField: This operation was cancelled."
          else do
            let rows' = map addClauIdxForOneRow rows                        -- [[MySQLValue]]
            let sqlstat1 = DS.fromString $ "update " ++ tree_target ++ " set tree = ? where serial_num = ?"
            oks <- executeMany conn sqlstat1 rows'
            putStrLn $ show (length oks) ++ " rows have been updated."             -- Only rows with their values changed are affected rows.

    closeStmt conn stmt
    close conn                       -- Close the connection.

{- Check the first element of every tuple.
 - If the first element is '(', then the sentence indicated by the second element might have finished operation 'addClauIdxToTreeField'; Otherwise, it might not.
 - If some sentences might finish the operation, return True; Otherwise, return False.
 - From the previous transition, 'prevRes' is True for some previous sentences finishing this operation, and False for no previous sentences finishes this operation.
 - So at the beginning, 'prevRes' should be False.
 -}
someFinished :: Bool -> [(Char, Int)] -> IO Bool
someFinished prevRes [] = return prevRes
someFinished prevRes (ci:cis)
    | fst ci == '[' = someFinished prevRes cis
    | fst ci == '(' = do
                        putStrLn $ "someFinished: Sentence " ++ show (snd ci) ++ " have finished operation 'addClauIdxToTreeField'."
                        someFinished True cis
    | otherwise = error "someFinished: Exception."

{- For [MySQLText tree, MySQLInt32 serial_num] in querying result from treebank, 'tree' is transformed into [[PhraCate]], then introduce clausal indices to
 - form [(Int, [PhraCate])]. 'serial_num' remain same.
 -}
addClauIdxForOneRow :: [MySQLValue] -> [MySQLValue]
addClauIdxForOneRow vs = [tree'', vs!!1]
    where
    treeStr = fromMySQLText (vs!!0)                            -- The field tree
    tree = map readPCList $ stringToList treeStr               -- [[PhraCate]]
    tree' = zip ([1..] :: [ClauIdx]) tree                      -- [(ClauIdx, [PhraCate])], namely [Tree]
    tree'' = (toMySQLText . listToString) $ map treeToString tree'
