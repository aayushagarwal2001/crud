{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DataKinds             #-}
module Main where
import ApiMovie
import BasicAuth
import Network.Wai.Handler.Warp
import Db  
import Servant.Server (Context ((:.), EmptyContext),serveWithContext)
main :: IO ()
main = do
       migrationScript
       run 8081 (serveWithContext basicAuthApi (authCheck :. EmptyContext) server10)

       