
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}
module BasicAuth where
import Data.Text                        (unpack)
import Servant.API.BasicAuth            (BasicAuthData (BasicAuthData))
import Servant.Server.Experimental.Auth()
import qualified Types as T
import Data.Text.Encoding (decodeUtf8)
import qualified Db as D
import Database.Persist (Entity(entityVal))
import Servant.Server                   (BasicAuthCheck (BasicAuthCheck),
                                         BasicAuthResult( Authorized
                                                        , Unauthorized, NoSuchUser
                                                        ), errBody)

-- | The result of authentication/authorization


authCheck :: BasicAuthCheck T.UserForAuth
authCheck =
  let check (BasicAuthData email  password ) = do
        x <- D.queryExecu5 $ unpack (decodeUtf8 email)
        case x of
         Just user -> if unpack (decodeUtf8 email) == D.userEmail (entityVal user) && unpack (decodeUtf8 password) == D.userPassword (entityVal user)
                    then return (Authorized (T.UserForAuth{T.emailAuth = D.userEmail (entityVal user) }) )
                    else return Unauthorized
         Nothing ->  return NoSuchUser
  in BasicAuthCheck check
