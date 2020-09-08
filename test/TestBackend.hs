{-# LANGUAGE OverloadedStrings #-}

module TestBackend where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (LoggingT)
import           Data.ByteString.Char8       (pack)
import           Data.Maybe                  (fromMaybe)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql        (ConnectionPool)
import           System.Environment          (lookupEnv)


createConnPool :: LoggingT IO ConnectionPool
createConnPool = do
    host <- liftIO $ lookupEnv "TEST_DB_HOST"
    let connStr = "host=" ++ fromMaybe "localhost" host  ++ " port=5432 user=postgres_test dbname=postgres_test password=postgres_test"
    createPostgresqlPool (pack connStr) 10
