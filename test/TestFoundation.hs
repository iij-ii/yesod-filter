{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module TestFoundation where

import           ClassyPrelude.Yesod
import           Database.Persist.Sql (ConnectionPool, runSqlPool)


mkYesodData "App" [parseRoutes|
/foos FoosR GET
|]

newtype App = App { appConnPool :: ConnectionPool }

instance Yesod App where
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = pure Nothing

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
