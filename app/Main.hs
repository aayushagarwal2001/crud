module Main (main) where
import ApiMovie
import Network.Wai.Handler.Warp
import Db 


main :: IO ()
main = do
       migrationScript
       run 8081 app1
       