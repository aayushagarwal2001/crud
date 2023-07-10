
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module ApiMovie where
import Prelude ()
import Prelude.Compat
import Control.Monad.Except
import Servant
import qualified Db as D
import Database.Esqueleto (Entity(entityVal))
import Types

type MovieGetAll        =  BasicAuth "foo-realm" UserForAuth :>  Get '[JSON] [Movie]
type MovieGetByName     =  BasicAuth "foo-realm" UserForAuth :>Capture "name" String :> Get '[JSON] [Movie]
type MoviePost          =  BasicAuth "foo-realm" UserForAuth :>ReqBody '[JSON] Movie :> Post '[JSON] Movie
type MovieUpdate        =  BasicAuth "foo-realm" UserForAuth :> "update" :> ReqBody '[JSON] Movie :> Post '[JSON] ()
type MovieDelete        =  BasicAuth "foo-realm" UserForAuth :> "delete" :>Capture "name" String :> Delete '[JSON] ()
type MovieApi           = MovieGetAll  :<|> MovieGetByName :<|> MoviePost :<|> MovieUpdate :<|> MovieDelete

----User

type UserRegister       =  "register" :> ReqBody '[JSON] User :> Post '[JSON] User
type UserLogin          =  "login"    :> QueryParam "email" String :> QueryParam "password" String :> Get '[JSON] String
type AddFavouriteMovie  =  BasicAuth "foo-realm" UserForAuth :>  Capture "movieName" String :> Post '[JSON] ()
type UserApi            = UserRegister :<|> UserLogin :<|> AddFavouriteMovie


server4 :: Server MovieApi
server4 = getAllMovies :<|> getMovieByName :<|> postMovie :<|> updateMovieByName :<|> deleteMovieByName
          where getAllMovies :: UserForAuth -> Handler [Movie]
                getAllMovies _ = do
                               map (\ b -> Movie {mname = D.movieMname $ entityVal b,rating = D.movieRating $ entityVal b,genre = D.movieGenre $ entityVal b}) <$> liftIO D.queryExecu
                getMovieByName :: UserForAuth -> String -> Handler [Movie]
                getMovieByName _ movieName = do
                                           x <- liftIO $ D.queryExecu1 movieName
                                           return $ map (\ b -> Movie {mname = D.movieMname $ entityVal b,rating = D.movieRating $ entityVal b,genre = D.movieGenre $ entityVal b}) x
                postMovie:: UserForAuth -> Movie -> Handler Movie
                postMovie _ movie = do
                                  x <-  liftIO $ D.queryExecu2 movie
                                  case x of
                                    Just _ -> return movie
                                    Nothing -> throwError err400

                updateMovieByName :: UserForAuth -> Movie -> Handler ()
                updateMovieByName  _ movie = do
                                            liftIO $ D.queryExecu3 movie
                deleteMovieByName :: UserForAuth -> String ->  Handler ()
                deleteMovieByName _ movieName = do
                                               liftIO $ D.queryExecu4 movieName


server5 :: Server UserApi
server5 = registerUser :<|> loginUser :<|> addFavMovie
        where
          registerUser :: User -> Handler User
          registerUser user = do
                            x <- liftIO $ D.queryExecu6 user
                            case x of
                              Just _ -> return user
                              Nothing -> throwError err400
          loginUser :: Maybe String -> Maybe String ->  Handler String
          loginUser email password = do
                                      case email of
                                        Just emailx -> case password of
                                                         Just passwordx -> do
                                                          x <- liftIO $ D.queryExecu5 emailx
                                                          case x of
                                                            Just user -> if D.userPassword (entityVal user) == passwordx then return $  D.userEmail (entityVal user) ++ ":" ++ D.userPassword (entityVal user) else throwError err401
                                                            Nothing -> throwError err400
                                                         Nothing -> throwError err400
                                        Nothing -> throwError err400

          addFavMovie :: UserForAuth -> String -> Handler ()
          addFavMovie user movie = do
                                   k <- liftIO $ D.queryExecu8 movie
                                   if k
                                    then  do
                                          y <- liftIO $ D.queryExecu5 (emailAuth user)
                                          case y of
                                            Just userdb -> liftIO $ D.queryExecu7 (((entityVal userdb){D.userFavouriteMovie = movie : D.userFavouriteMovie (entityVal userdb) }))
                                            Nothing   -> throwError err401

                                    else throwError err400
                                   



type CombinedAPI = "user" :> UserApi
              :<|> ("movie" :> MovieApi)

server10 :: Server CombinedAPI
server10 = server5 :<|> server4

basicAuthApi :: Proxy CombinedAPI
basicAuthApi = Proxy