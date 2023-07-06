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
import Control.Monad.IO.Unlift
import qualified Types as T


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Movie
      mname String 
      genre String
      rating Double
      Primary mname
      deriving Show

User
      name  String
      age   Int
      email String
      registration_date Day
      Primary email
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

insertMovie :: MonadIO m => T.Movie ->ReaderT SqlBackend m (Maybe ())
insertMovie m = do
      insertUnique_ $ Movie (T.mname m) (T.genre m) (T.rating m)



updateMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m,
 PersistQueryWrite backend) => T.Movie -> ReaderT backend m ()
updateMovie movie = updateWhere [MovieMname ==. T.mname movie] [MovieRating =. T.rating movie]
-- dbConnect = do
--             runStderrLoggingT $ withPostgresqlPool "host=localhost dbname=test_db user=root password=root port=5432" 10 (liftSqlPersistMPool fetchAllMovies)



deleteMovie :: (MonadIO m, BackendCompatible SqlBackend backend,
 PersistQueryWrite backend, PersistUniqueWrite backend) =>String -> ReaderT backend m ()
deleteMovie movieName =  E.delete $
                         E.from $ \p -> do
                         E.where_ (p E.^. MovieMname E.==. E.val movieName)
                         
queryExecu :: IO [Entity Movie]
queryExecu = do
             runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
              liftIO (runSqlPersistM fetchAllMovies b))





queryExecu1 ::  String -> IO [Entity Movie]
queryExecu1 movieName = do
                  runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
                       liftIO (runSqlPersistM (fetchOneByName movieName) b))



queryExecu2 ::  T.Movie -> IO (Maybe ())
queryExecu2 movie = do
                    runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
                        runSqlConn (insertMovie movie) b)

queryExecu3 :: T.Movie -> IO ()
queryExecu3  movie = do
                    runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
                        runSqlConn (updateMovie movie) b)

queryExecu4 :: String -> IO ()
queryExecu4 name = do 
                   runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
                        runSqlConn (deleteMovie name) b)
                  

migrationScript :: IO ()
migrationScript = do
                  runNoLoggingT $ withPostgresqlConn "host=localhost dbname=test_db user=root password=root port=5432" (\b -> do
                        runSqlConn (runMigration migrateAll) b)
