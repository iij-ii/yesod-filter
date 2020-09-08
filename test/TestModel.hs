{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module TestModel where

import           ClassyPrelude.Yesod
import           Data.Time           (TimeOfDay)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo json
    ctxt Text
    cint Int
    cdbl Double
    cbol Bool
    cday Day
    ctod TimeOfDay
    cutm UTCTime
    mtxt Text Maybe
    mint Int Maybe
    mdbl Double Maybe
    mbol Bool Maybe
    mday Day Maybe
    mtod TimeOfDay Maybe
    mutm UTCTime Maybe
    deriving Eq
    deriving Show
|]
