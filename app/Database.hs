{-# LANGUAGE OverloadedStrings #-}

-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power
-- All rights reserved.

module Database (
  fromMySQLInt8,               -- MySQLValue (MySQLInt8) -> Int
  fromMySQLInt16U,             -- MySQLValue (MySQLInt16U) -> Int
  fromMySQLInt32U,             -- MySQLValue (MySQLInt32U) -> Int
  fromMySQLInt32,              -- MySQLValue (MySQLInt32) -> Int
  fromMySQLInt64,              -- MySQLValue (MySQLInt64) -> Int
  fromMySQLText,               -- MySQLValue (MySQLText) -> String
  fromMySQLNullText,           -- MySQLValue (MySQLNull)-> String
  fromMySQLNullVarchar,        -- MySQLValue (MySQLNull) -> String
  toMySQLInt8,                 -- Int -> MySQLValue (MySQLInt8)
  toMySQLInt16U,               -- Int -> MySQLValue (MySQLInt16U)
  toMySQLInt32U,               -- Int -> MySQLValue (MySQLInt32U)
  toMySQLInt32,                -- Int -> MySQLValue (MySQLInt32)
  toMySQLInt64,                -- Int -> MySQLValue (MySQLInt64)
  toMySQLText,                 -- String -> MySQLValue (MySQLText)
  toMySQLNullText,             -- MySQLValue (MySQLNull)
  toMySQLNullVarchar,          -- MySQLValue (MySQLNull)
  getColumnDB,                 -- ColumnDef -> ByteString
  getColumnTable,              -- ColumnDef -> ByteString
  getColumnOrigTable,          -- ColumnDef -> ByteString
  getColumnName,               -- ColumnDef -> ByteString
  getColumnOrigName,           -- ColumnDef -> ByteString
  getColumnCharSet,            -- ColumnDef -> Word16
  getColumnLength,             -- ColumnDef -> Word32
  getColumnType,               -- ColumnDef -> String
  getColumnFlags,              -- ColumnDef -> Word16
  getColumnDecimals,           -- ColumnDef -> Word8
  getOkAffectedRows,           -- OK -> Int
  getOkLastInsertID,           -- OK -> Int
  getOkStatus,                 -- OK -> Word16
  getOkWarningCnt,             -- OK -> Word16
  getConn,                     -- IO MySQLConn
  getConnByUserWqj,            -- IO MySQLConn
  getConnByUserWqjAtLocal      -- IO MySQLConn
  ) where

import           Control.Monad
import           System.IO
import qualified System.IO.Streams as S
import           Database.MySQL.Base
import           Data.List.Utils
import           Data.List as DL hiding (words)
import           Data.Tuple.Utils
import           Data.Int
import           Data.Text as DT hiding (length, map, head, last, foldl, words)
import           Data.Word
import           Data.ByteString.Char8 as BC hiding (putStrLn,readFile,map,words,head,last)
import           Utils

fromMySQLInt8 :: MySQLValue -> Int
fromMySQLInt8 (MySQLInt8 a) = read (show a) :: Int
fromMySQLInt8 _ = error "fromMySQLInt8: Parameter error."

fromMySQLInt16U :: MySQLValue -> Int
fromMySQLInt16U (MySQLInt16U a) = read (show a) :: Int
fromMySQLInt16U _ = error "fromMySQLInt16U: Parameter error."

fromMySQLInt32U :: MySQLValue -> Int
fromMySQLInt32U (MySQLInt32U a) = read (show a) :: Int
fromMySQLInt32U _ = error "fromMySQLInt32U: Parameter error."

fromMySQLInt32 :: MySQLValue -> Int
fromMySQLInt32 (MySQLInt32 a) = read (show a) :: Int
fromMySQLInt32 _ = error "fromMySQLInt32: Parameter error."

fromMySQLInt64 :: MySQLValue -> Int
fromMySQLInt64 (MySQLInt64 a) = read (show a) :: Int
fromMySQLInt64 _ = error "fromMySQLInt64: Parameter error."

fromMySQLText :: MySQLValue -> String
fromMySQLText (MySQLText a)
    | str!!0 == '\"' = DL.init $ DL.tail str     -- Throw away char " at head and rear.
    | otherwise = str
    where
      str = DT.unpack a
fromMySQLText _ = error "fromMySQLText: Parameter error."

fromMySQLNullText :: MySQLValue -> String
fromMySQLNullText MySQLNull = ""
fromMySQLNullText _ = error "fromMySQLNullText: Parameter error."

fromMySQLNullVarchar :: MySQLValue -> String
fromMySQLNullVarchar MySQLNull = ""
fromMySQLNullVarchar _ = error "fromMySQLNullVarchar: Parameter error."

toMySQLInt8 :: Int -> MySQLValue
toMySQLInt8 v = MySQLInt8 (read (show v) :: Int8)

toMySQLInt16U :: Int -> MySQLValue
toMySQLInt16U v = MySQLInt16U (read (show v) :: Word16)

toMySQLInt32U :: Int -> MySQLValue
toMySQLInt32U v = MySQLInt32U (read (show v) :: Word32)

toMySQLInt32 :: Int -> MySQLValue
toMySQLInt32 v = MySQLInt32 (read (show v) :: Int32)

toMySQLInt64 :: Int -> MySQLValue
toMySQLInt64 v = MySQLInt64 (read (show v) :: Int64)

toMySQLText :: String -> MySQLValue
toMySQLText v = MySQLText (DT.pack v)

toMySQLNullText :: MySQLValue
toMySQLNullText = MySQLNull

toMySQLNullVarchar :: MySQLValue
toMySQLNullVarchar = MySQLNull

getColumnDB :: ColumnDef -> ByteString
getColumnDB (ColumnDef db _ _ _ _ _ _ _ _ _) = read (show db) :: ByteString

getColumnTable :: ColumnDef -> ByteString
getColumnTable (ColumnDef _ tb _ _ _ _ _ _ _ _) = read (show tb) :: ByteString

getColumnOrigTable :: ColumnDef -> ByteString
getColumnOrigTable (ColumnDef _ _ ot _ _ _ _ _ _ _) = read (show ot) :: ByteString

getColumnName :: ColumnDef -> ByteString
getColumnName (ColumnDef _ _ _ na _ _ _ _ _ _) = read (show na) :: ByteString

getColumnOrigName :: ColumnDef -> ByteString
getColumnOrigName (ColumnDef _ _ _ _ on _ _ _ _ _) = read (show on) :: ByteString

getColumnCharSet :: ColumnDef -> Word16
getColumnCharSet (ColumnDef _ _ _ _ _ cs _ _ _ _) = read (show cs) :: Word16

getColumnLength :: ColumnDef -> Word32
getColumnLength (ColumnDef _ _ _ _ _ _ ln _ _ _) = read (show ln) :: Word32

getColumnType :: ColumnDef -> String
getColumnType (ColumnDef _ _ _ _ _ _ _ tp _ _) = show tp

getColumnFlags :: ColumnDef -> Word16
getColumnFlags (ColumnDef _ _ _ _ _ _ _ _ fl _) = read (show fl) :: Word16

getColumnDecimals :: ColumnDef -> Word8
getColumnDecimals (ColumnDef _ _ _ _ _ _ _ _ _ de) = read (show de) :: Word8

getOkAffectedRows :: OK -> Int
getOkAffectedRows (OK okAffectedRow _ _ _) = okAffectedRow

getOkLastInsertID :: OK -> Int
getOkLastInsertID (OK _ okLastInsertID _ _) = okLastInsertID

getOkStatus :: OK -> Word16
getOkStatus (OK _ _ okStatus _) = okStatus

getOkWarningCnt :: OK -> Word16
getOkWarningCnt (OK _ _ _ okWarningCnt) = okWarningCnt

-- Get a connection to MySQL database according to a configuration file.
getConn :: IO MySQLConn
getConn = do
    confInfo <- readFile "mysql_config"      -- Read the local configuration file
    let confTupleSeq = [(head keyValue, last keyValue)| keyValue <- map (splitAtDeli ':') (words confInfo)]
    let host = DL.foldl (++) "" [snd tp | tp <- confTupleSeq, fst tp == "Host"]    -- Change [String] as String
    let user = DL.foldl (++) "" [snd tp | tp <- confTupleSeq, fst tp == "User"]
    let password = DL.foldl (++) "" [snd tp | tp <- confTupleSeq, fst tp == "Password"]
    let database = DL.foldl (++) "" [snd tp | tp <- confTupleSeq, fst tp == "Database"]
--  putStrLn $ "host:" ++ host ++ ", user:" ++ user ++ ", password:" ++ password ++ ", database:" ++ database

    connect defaultConnectInfo {
      ciHost = host,
      ciUser = BC.pack user,             -- Change String to ByteString
      ciPassword = BC.pack password,
      ciDatabase = BC.pack database
    }

-- Get a connection to database 'ccg4c' with user 'wqj'.
getConnByUserWqj :: IO MySQLConn
getConnByUserWqj = connect defaultConnectInfo {
    ciHost = "125.219.93.62",
--    ciHost = "127.0.0.1",
    ciUser = "wqj",
    ciPassword = "wqj",
    ciDatabase = "ccg4c"
    }

-- Get a connection to database 'ccg4c' with user 'wqj' at local host.
getConnByUserWqjAtLocal :: IO MySQLConn
getConnByUserWqjAtLocal = connect defaultConnectInfo {
    ciHost = "127.0.0.1",
    ciUser = "wqj",
    ciPassword = "wqj",
    ciDatabase = "ccg4c"
    }
