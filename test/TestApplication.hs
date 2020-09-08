{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestApplication
    ( module TestApplication
    , module X
    ) where

import           ClassyPrelude                        as X hiding (Handler, delete, deleteBy)
import           Control.Monad.Logger                 (runLoggingT)
import           Data.Default                         (def)
import           Database.Persist                     as X hiding (get)
import           Database.Persist.Sql                 (SqlPersistM, runMigration,
                                                       runSqlPersistMPool, runSqlPool)
import           Network.Wai.Middleware.RequestLogger (IPAddrSource (FromSocket),
                                                       OutputFormat (Apache), mkRequestLogger,
                                                       outputFormat)
import           Test.Hspec                           as X
import           Yesod.Core                           (defaultMakeLogger,
                                                       defaultMessageLoggerSource,
                                                       defaultShouldLogIO, mkYesodDispatch)
import           Yesod.Test                           as X

import           TestBackend                          (createConnPool)
import           TestFoundation                       as X
import           TestHandler                          (getFoosR)
import           TestModel                            as X


runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

mkYesodDispatch "App" resourcesApp

makeFoundation :: IO App
makeFoundation = do
    logger <- defaultMakeLogger
    let logFunc = defaultMessageLoggerSource defaultShouldLogIO logger
    pool <- runLoggingT createConnPool logFunc
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    pure $ App pool

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    foundation <- makeFoundation
    logWare <- liftIO $ mkRequestLogger def { outputFormat = Apache FromSocket }
    return (foundation, logWare)
