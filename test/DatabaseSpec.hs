-- Copyright (c) 2019-2025 China University of Water Resources and Electric Power
-- All rights reserved.

module DatabaseSpec where

import           Database
import           Database.MySQL.Base
import qualified Data.String as DS
import           Test.Hspec

spec :: Spec
spec = do
  describe "Database" $ do
    it "create table if not exists test (id int, name char(8)), okStatus shouldBe 2." $ do
      conn <- getConnByUserWqj
      let sqlstat = DS.fromString "create table if not exists test (id int, name char(8))"
      stmt <- prepareStmt conn sqlstat
      ok <- executeStmt conn stmt []
      getOkStatus ok `shouldBe` 2

    it "insert into test set id=1, name=\"QJWang\", okStatus shouldBe 2." $ do
      conn <- getConnByUserWqj
      let sqlstat = DS.fromString "insert into test set id=1, name=\"QJWang\""
      stmt <- prepareStmt conn sqlstat
      ok <- executeStmt conn stmt []
      getOkStatus ok `shouldBe` 2

    it "update test set id=1, name=\"QJWang\", okStatus shouldBe 34." $ do
      conn <- getConnByUserWqj
      let sqlstat = DS.fromString "update test set id=1, name=\"QJWang\""
      stmt <- prepareStmt conn sqlstat
      ok <- executeStmt conn stmt []
      getOkStatus ok `shouldBe` 34
