{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module ApiMovie where
import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Servant
import qualified Db as D
import Database.Esqueleto (Entity(entityVal), liftPersist)
import Types
import Network.HTTP.Types.Status (ok200)


type MovieGetAll      = "movie" :> Get '[JSON] [Movie]
type MovieGetByName   = "movie" :> Capture "name" String :> Get '[JSON] [Movie]
type MoviePost        = "movie" :> ReqBody '[JSON] Movie :> Post '[JSON] Movie
type MovieUpdate      = "movie" :> "update" :> ReqBody '[JSON] Movie :> Post '[JSON] ()
type MovieDelete      = "movie" :>  "delete" :> Capture "name" String :> Delete '[JSON] ()
type MovieApi         = MovieGetAll  :<|> MovieGetByName :<|> MoviePost :<|> MovieUpdate :<|> MovieDelete



server4 :: Server MovieApi
server4 = getAllMovies :<|> getMovieByName :<|> postMovie :<|> updateMovieByName :<|> deleteMovieByName
          where getAllMovies :: Handler [Movie]
                getAllMovies = do
                               map (\ b -> Movie {mname = D.movieMname $ entityVal b,rating = D.movieRating $ entityVal b,genre = D.movieGenre $ entityVal b}) <$> liftIO D.queryExecu
                getMovieByName :: String -> Handler [Movie]
                getMovieByName movieName = do
                                           x <- liftIO $ D.queryExecu1 movieName
                                           return $ map (\ b -> Movie {mname = D.movieMname $ entityVal b,rating = D.movieRating $ entityVal b,genre = D.movieGenre $ entityVal b}) x
                postMovie:: Movie -> Handler Movie
                postMovie movie = do
                                  x <-  liftIO $ D.queryExecu2 movie
                                  case x of
                                    Just _ -> return movie
                                    Nothing -> throwError err400
                
                updateMovieByName :: Movie -> Handler ()
                updateMovieByName  movie = do
                                            liftIO $ D.queryExecu3 movie
                deleteMovieByName :: String -> Handler ()
                deleteMovieByName movieName = do
                                               liftIO $ D.queryExecu4 movieName
userAPI1 :: Proxy MovieApi
userAPI1 = Proxy
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI1 server4
-- server2 :: Server



