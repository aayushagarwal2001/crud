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
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import Servant
import qualified Db as D
import Database.Esqueleto (Entity(entityVal), liftPersist)
import Control.Monad.IO.Unlift (UnliftIO(unliftIO), MonadUnliftIO (withRunInIO))
import Types

type MovieGetAll      = "movie" :> Get '[JSON] [Movie]
type MovieGetByName   = "movie" :> Capture "name" String :> Get '[JSON] [Movie]
type MoviePost        = "movie" :> ReqBody '[JSON] Movie :> Post '[JSON] Movie
type MovieApi         = MovieGetAll  :<|> MovieGetByName :<|> MoviePost



server4 :: Server MovieApi
server4 = getAllMovies :<|> getMovieByName :<|> postMovie
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
                                    
userAPI1 :: Proxy MovieApi
userAPI1 = Proxy
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI1 server4
-- server2 :: Server



