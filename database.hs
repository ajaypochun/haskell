/* Creating Database – using SQLite   - Not tested due to technical issues*/


{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
module Main where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)

-- rawSql imports.
import Database.Persist.Sql (rawQuery, insert)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Temperature:
   date String
   temperature int
   deriving Show
[]main = runSqlite ":memory:" $ do
    buildDb
    dumpTable

buildDb = do
    runMigrationSilent 
    insert $ {"temperatures":[
	{"date":"2015-02-28T20:16:12+00:00", "temperature":0},
	{"date":"2015-01-01T21:46:55+00:00", "temperature":2}, 
	{"date":"2015-04-08T21:46:53+00:00", "temperature":3},
	{"date":"2015-04-09T21:46:01+00:00", "temperature":4},
	{"date":"2015-04-10T21:46:40+00:00", "temperature":5},
	{"date":"2015-04-11T21:46:36+00:00", "temperature":6},
	{"date":"2015-04-12T20:36:25+00:00", "temperature":7}
]}


dumpTable = rawQuery "select * from Table" [] $$ CL.mapM_ (liftIO . print)

