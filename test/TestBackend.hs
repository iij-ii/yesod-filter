{-# LANGUAGE OverloadedStrings #-}

module TestBackend where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (LoggingT)
import           Data.Maybe             (fromMaybe)
import           Database.Persist.MySQL (connectDatabase, connectHost, connectPassword, connectPort,
                                         connectUser, createMySQLPool, defaultConnectInfo)
import           Database.Persist.Sql   (ConnectionPool)
import           System.Environment     (lookupEnv)


createConnPool :: LoggingT IO ConnectionPool
createConnPool = do
    host <- liftIO $ lookupEnv "TEST_DB_HOST"
    let connInfo = defaultConnectInfo
            { connectHost = fromMaybe "127.0.0.1" host
            , connectPort = 3306
            , connectUser = "mysql_test"
            , connectPassword = "mysql_test"
            , connectDatabase = "mysql_test"
            }
    createMySQLPool connInfo 10
