{-# LANGUAGE OverloadedStrings #-}

module TestBackend where

import           Control.Monad.Logger    (LoggingT)
import           Database.Persist.Sql    (ConnectionPool)
import           Database.Persist.Sqlite (createSqlitePool)


createConnPool :: LoggingT IO ConnectionPool
createConnPool = createSqlitePool "yesod-filter_test.sqlite3" 10
