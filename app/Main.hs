module Main (main) where
import ApiMovie
import Network.Wai.Handler.Warp
import Database.Persist.Postgresql
import Control.Monad.Cont (MonadIO(liftIO))


main :: IO ()
main = do
       run 8081 app1
       