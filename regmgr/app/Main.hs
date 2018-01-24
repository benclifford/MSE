{-# Language DataKinds #-}
{-# Language TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)

import Servant
import Servant.API

import Lib

type OverviewAPI =
  "overview" :> Get '[PlainText] String

server1 :: Server OverviewAPI
server1 = return "hi"

regmgrAPI :: Proxy OverviewAPI
regmgrAPI = Proxy

app = serve regmgrAPI server1

main :: IO ()
main = do
  putStrLn "regmgr start"

  run 8080 app

  putStrLn "regmgr end"
