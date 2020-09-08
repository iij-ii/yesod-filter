{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Yesod.Filter.TestTypes where

import           Data.Text           (Text)
import           Data.Time           (Day, TimeOfDay, UTCTime)
import           Database.Persist    (Filter (Filter), FilterValue (FilterValue),
                                      PersistFilter (Eq, Ge, Gt, Le, Lt, Ne),
                                      SelectOpt (Asc, Desc, LimitTo, OffsetBy))
import           Database.Persist.TH (mkPersist, persistLowerCase, sqlSettings)


newtype FV typ = FV (FilterValue typ)

instance Eq typ => Eq (FV typ) where
    (==) (FV (FilterValue v1)) (FV (FilterValue v2)) = v1 == v2
    (==) _ _                                         = undefined

instance Show typ => Show (FV typ) where
    show (FV (FilterValue v)) = "FilterValue " ++ show v
    show _                    = undefined

newtype PF = PF PersistFilter

instance Eq PF where
    (==) (PF Eq) (PF Eq) = True
    (==) (PF Ne) (PF Ne) = True
    (==) (PF Gt) (PF Gt) = True
    (==) (PF Lt) (PF Lt) = True
    (==) (PF Ge) (PF Ge) = True
    (==) (PF Le) (PF Le) = True
    (==) _ _             = False

mkPersist sqlSettings [persistLowerCase|
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

-- The following was generated by TestTypes/geninst.hs

instance Eq (Filter Foo) where
    (==)  (Filter FooId v1 f1) (Filter FooId v2 f2)     = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooCtxt v1 f1) (Filter FooCtxt v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooCint v1 f1) (Filter FooCint v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooCdbl v1 f1) (Filter FooCdbl v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooCbol v1 f1) (Filter FooCbol v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooCday v1 f1) (Filter FooCday v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooCtod v1 f1) (Filter FooCtod v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooCutm v1 f1) (Filter FooCutm v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooMtxt v1 f1) (Filter FooMtxt v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooMint v1 f1) (Filter FooMint v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooMdbl v1 f1) (Filter FooMdbl v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooMbol v1 f1) (Filter FooMbol v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooMday v1 f1) (Filter FooMday v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooMtod v1 f1) (Filter FooMtod v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==)  (Filter FooMutm v1 f1) (Filter FooMutm v2 f2) = (FV v1, PF f1) == (FV v2, PF f2)
    (==) _ _                                            = undefined

instance Show (Filter Foo) where
    show (Filter FooId v f)   = "Filter FooId (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooCtxt v f) = "Filter FooCtxt (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooCint v f) = "Filter FooCint (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooCdbl v f) = "Filter FooCdbl (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooCbol v f) = "Filter FooCbol (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooCday v f) = "Filter FooCday (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooCtod v f) = "Filter FooCtod (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooCutm v f) = "Filter FooCutm (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooMtxt v f) = "Filter FooMtxt (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooMint v f) = "Filter FooMint (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooMdbl v f) = "Filter FooMdbl (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooMbol v f) = "Filter FooMbol (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooMday v f) = "Filter FooMday (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooMtod v f) = "Filter FooMtod (" ++ show (FV v) ++ ") " ++ show f
    show (Filter FooMutm v f) = "Filter FooMutm (" ++ show (FV v) ++ ") " ++ show f
    show _                    = undefined

instance Eq (SelectOpt Foo) where
    (==) (Asc FooId) (Asc FooId)       = True
    (==) (Desc FooId) (Desc FooId)     = True
    (==) (Asc FooCtxt) (Asc FooCtxt)   = True
    (==) (Desc FooCtxt) (Desc FooCtxt) = True
    (==) (Asc FooCint) (Asc FooCint)   = True
    (==) (Desc FooCint) (Desc FooCint) = True
    (==) (Asc FooCdbl) (Asc FooCdbl)   = True
    (==) (Desc FooCdbl) (Desc FooCdbl) = True
    (==) (Asc FooCbol) (Asc FooCbol)   = True
    (==) (Desc FooCbol) (Desc FooCbol) = True
    (==) (Asc FooCday) (Asc FooCday)   = True
    (==) (Desc FooCday) (Desc FooCday) = True
    (==) (Asc FooCtod) (Asc FooCtod)   = True
    (==) (Desc FooCtod) (Desc FooCtod) = True
    (==) (Asc FooCutm) (Asc FooCutm)   = True
    (==) (Desc FooCutm) (Desc FooCutm) = True
    (==) (Asc FooMtxt) (Asc FooMtxt)   = True
    (==) (Desc FooMtxt) (Desc FooMtxt) = True
    (==) (Asc FooMint) (Asc FooMint)   = True
    (==) (Desc FooMint) (Desc FooMint) = True
    (==) (Asc FooMdbl) (Asc FooMdbl)   = True
    (==) (Desc FooMdbl) (Desc FooMdbl) = True
    (==) (Asc FooMbol) (Asc FooMbol)   = True
    (==) (Desc FooMbol) (Desc FooMbol) = True
    (==) (Asc FooMday) (Asc FooMday)   = True
    (==) (Desc FooMday) (Desc FooMday) = True
    (==) (Asc FooMtod) (Asc FooMtod)   = True
    (==) (Desc FooMtod) (Desc FooMtod) = True
    (==) (Asc FooMutm) (Asc FooMutm)   = True
    (==) (Desc FooMutm) (Desc FooMutm) = True
    (==) (OffsetBy n1) (OffsetBy n2)   = n1 == n2
    (==) (LimitTo n1) (LimitTo n2)     = n1 == n2
    (==) _ _                           = False

instance Show (SelectOpt Foo) where
    show (Asc FooId)    = "Asc FooId"
    show (Desc FooId)   = "Desc FooId"
    show (Asc FooCtxt)  = "Asc FooCtxt"
    show (Desc FooCtxt) = "Desc FooCtxt"
    show (Asc FooCint)  = "Asc FooCint"
    show (Desc FooCint) = "Desc FooCint"
    show (Asc FooCdbl)  = "Asc FooCdbl"
    show (Desc FooCdbl) = "Desc FooCdbl"
    show (Asc FooCbol)  = "Asc FooCbol"
    show (Desc FooCbol) = "Desc FooCbol"
    show (Asc FooCday)  = "Asc FooCday"
    show (Desc FooCday) = "Desc FooCday"
    show (Asc FooCtod)  = "Asc FooCtod"
    show (Desc FooCtod) = "Desc FooCtod"
    show (Asc FooCutm)  = "Asc FooCutm"
    show (Desc FooCutm) = "Desc FooCutm"
    show (Asc FooMtxt)  = "Asc FooMtxt"
    show (Desc FooMtxt) = "Desc FooMtxt"
    show (Asc FooMint)  = "Asc FooMint"
    show (Desc FooMint) = "Desc FooMint"
    show (Asc FooMdbl)  = "Asc FooMdbl"
    show (Desc FooMdbl) = "Desc FooMdbl"
    show (Asc FooMbol)  = "Asc FooMbol"
    show (Desc FooMbol) = "Desc FooMbol"
    show (Asc FooMday)  = "Asc FooMday"
    show (Desc FooMday) = "Desc FooMday"
    show (Asc FooMtod)  = "Asc FooMtod"
    show (Desc FooMtod) = "Desc FooMtod"
    show (Asc FooMutm)  = "Asc FooMutm"
    show (Desc FooMutm) = "Desc FooMutm"
    show (OffsetBy n)   = "OffsetBy " ++ show n
    show (LimitTo n)    = "LimitTo " ++ show n
