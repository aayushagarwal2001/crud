{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Db where
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Reader
import Data.Time (Day (ModifiedJulianDay))
import qualified Database.Esqueleto as E
import Control.Monad.Logger
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Movie
      mname String 
      genre String
      rating Double
      deriving Show

User
      name  String
      age   Int
      email String
      registration_date Day
      deriving Show
|]

loadData :: MonadIO m => ReaderT SqlBackend m ()
loadData = do
  insert_ $ Movie "Spider-man:Far from home" "action" 4.8
  insert_ $ User  "Aayush" 22 "aayush.k@gn.com" (ModifiedJulianDay 100000)




fetchAllMovies :: SqlPersistM [Entity Movie]
fetchAllMovies = E.select $
                   E.from $ \movie -> do
                    return movie





fetchOneByName :: String -> SqlPersistM [Entity Movie]
fetchOneByName movieName =  E.select $
                             E.from $ \movie -> do
                              E.where_ (movie E.^. MovieMname E.==. E.val movieName )
                              E.limit 1
                              return movie


-- dbConnect = do
--             runStderrLoggingT $ withPostgresqlPool "host=localhost dbname=test_db user=root password=root port=5432" 10 (liftSqlPersistMPool fetchAllMovies)



queryExecu :: IO [Entity Movie]
queryExecu = do
             runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
              liftIO (runSqlPersistM fetchAllMovies b))





queryExecu1 ::  String -> IO [Entity Movie]
queryExecu1 movieName = do
                  runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
                       liftIO (runSqlPersistM (fetchOneByName movieName) b))



