{-# Language OverloadedStrings #-}
module Main where

import Control.Lens
import Data.Text (pack)
import Network.Wreq
import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  putStrLn "MSE/OSM gateway authorise"
  args <- getArgs
  let apiId = args !! 0
  let token = args !! 1
  let email = args !! 2
  let password = args !! 3
  putStrLn $ "API ID: " ++ apiId
  putStrLn $ "Token: " ++ token
  putStrLn $ "Email: " ++ email
  putStrLn $ "Password: " ++ password

  -- now query users.php?action=authorise&...etc...
  -- with parts an array:
  --    password
  --    email

  -- presumably token and api ID are globals in the PHP code.
  -- we'l get back a userid and a secret, and those will need to
  -- be supplied as parameters in future along with apiid and token.
  
  let url = "https://www.onlinescoutmanager.co.uk/users.php"

  let opts = defaults & param "action" .~ ["authorise"]

  let postData = [ "apiid" := apiId
                 , "token" := token
                 , "password" := password
                 , "email" := email
                 ]
 
  r <- postWith opts url postData

  print r

  print $ r ^.. responseBody
