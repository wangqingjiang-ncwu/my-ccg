{-# LANGUAGE OverloadedStrings #-}

-- Copyright (c) 2019-2021 China University of Water Resources and Electric Power
-- All rights reserved.

module Database (
  fromMySQLInt8,               -- MySQLValue (MySQLInt8) -> Int
  fromMySQLInt32U,             -- MySQLValue (MySQLInt32U) -> Int
  fromMySQLInt32,              -- MySQLValue (MySQLInt32) -> Int
  fromMySQLText,               -- MySQLValue (MySQLText) -> String
  fromMySQLNullText,           -- MySQLValue (MySQLNull)-> String
  fromMySQLNullVarchar,        -- MySQLValue (MySQLNull) -> String
  toMySQLInt8,                 -- Int -> MySQLValue (MySQLInt8)
  toMySQLInt32U,               -- Int -> MySQLValue (MySQLInt32U)
  toMySQLInt32,                -- Int -> MySQLValue (MySQLInt32)
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
  ) where

import           Control.Monad
import           System.IO
import qualified System.IO.Streams as S
import           Database.MySQL.Base
import           Data.List.Utils
import           Data.List
import           Data.Tuple.Utils
import           Data.Int
import           Data.Text hiding (length)
import           Data.Word
import           Data.ByteString hiding (putStr,putStrLn,length,pack,unpack)

fromMySQLInt8 :: MySQLValue -> Int
fromMySQLInt8 (MySQLInt8 a) = read (show a) :: Int

fromMySQLInt32U :: MySQLValue -> Int
fromMySQLInt32U (MySQLInt32U a) = read (show a) :: Int

fromMySQLInt32 :: MySQLValue -> Int
fromMySQLInt32 (MySQLInt32 a) = read (show a) :: Int

fromMySQLText :: MySQLValue -> String
fromMySQLText (MySQLText a) = unpack a

fromMySQLNullText :: MySQLValue -> String
fromMySQLNullText MySQLNull = ""

fromMySQLNullVarchar :: MySQLValue -> String
fromMySQLNullVarchar MySQLNull = ""

toMySQLInt8 :: Int -> MySQLValue
toMySQLInt8 v = MySQLInt8 (read (show v) :: Int8)

toMySQLInt32U :: Int -> MySQLValue
toMySQLInt32U v = MySQLInt32U (read (show v) :: Word32)

toMySQLInt32 :: Int -> MySQLValue
toMySQLInt32 v = MySQLInt32 (read (show v) :: Int32)

toMySQLText :: String -> MySQLValue
toMySQLText v = MySQLText (pack v)

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

-- Get a connection with given database.
getConn :: IO MySQLConn
getConn = connect defaultConnectInfo {
    ciHost = "125.219.93.78",
    ciUser = "graduate",
    ciPassword = "graduate",
    ciDatabase = "ccg4c"
    }
