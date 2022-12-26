{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Data.Text
import Servant.API
import Servant.Server
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import System.Random
import Network.Wai.Middleware.Cors
import Network.URI.Encode


type OurBlogAPI =  "hello"  :> Get '[JSON] String
              :<|> "add"    :> ReqBody '[JSON] PostData :> Post '[JSON] String
              :<|> "add2"    :> Capture "title" String :> Capture "img" String :> Capture "text" String :> Capture "descr" String :> Post '[JSON] String
              :<|> "delete" :> ReqBody '[JSON] DeleteData :> Post '[JSON] String
              :<|> "delete2" :> Capture "id" Int :> Post '[JSON] String
              :<|> "post"   :> Capture "x" Int :> Post '[JSON] String
              :<|> "all"    :> Post '[JSON] String
              :<|> "print"  :> Get '[JSON] String


data PostData = PostData
  { text :: String
  , img :: String
  , title :: String
  , descr :: String
} deriving Generic
instance FromJSON PostData
instance ToJSON PostData

data DeleteData = DeleteData
  { postDel :: Int
} deriving Generic
instance FromJSON DeleteData
instance ToJSON DeleteData


addNewPost :: PostData -> Handler String
addNewPost post = do
          pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
          g <- newStdGen
          let a :: Int = fst $ randomR (10000000, 99999999) g
          e <- liftIO $ access pipe master "servantAPI" (addPost (text post) (img post) (title post) (descr post) a)
          liftIO $ close pipe
          return (show a)
addPost :: String -> String -> String -> String -> Int -> Action IO [Database.MongoDB.Value]
addPost text img title descr pi = insertMany "post" [
   ["text" =: text, "img" =: img, "title" =: title, "descr" =: descr, "postId" =: pi]]
   
addNewPost2 :: String -> String -> String -> String -> Handler String
addNewPost2 title img text descr = do
          pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
          g <- newStdGen
          let a :: Int = fst $ randomR (10000000, 99999999) g
          e <- liftIO $ access pipe master "servantAPI" (addPost text (Network.URI.Encode.decode img) title descr a)
          liftIO $ close pipe
          return (show a)


deletePost :: DeleteData -> Handler String
deletePost post = do
          pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
          e <- liftIO $ access pipe master "servantAPI" (delPost (postDel post))
          liftIO $ close pipe
          return (show $ postDel post)
delPost :: Int -> Action IO ()
delPost pi = Database.MongoDB.deleteOne (select ["postId" =: pi] "post")


deletePost2 :: Int -> Handler String
deletePost2 postId = do
          pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
          e <- liftIO $ access pipe master "servantAPI" (delPost postId)
          liftIO $ close pipe
          return (show $ postId)


getPostById :: Int -> Handler String
getPostById postId = do
          pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
          e <- liftIO $ access pipe master "servantAPI" (getPost postId)
          liftIO $ close pipe
          return (show $ e)
getPost :: Int -> Action IO (Maybe Document)
getPost pi =  Database.MongoDB.findOne (select ["postId" =: pi] "post") {project = ["_id" =: 0]}


getAllPosts :: Handler String
getAllPosts = do
          pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
          e <- liftIO $ access pipe master "servantAPI" getAll
          liftIO $ close pipe
          return (show $ e)
getAll :: Action IO [Document]
getAll = rest =<< Database.MongoDB.find (select [] "post") {project = ["_id" =: 0, "text" =: 0]}


allTeams :: Action IO [Document]
allTeams = rest =<< Database.MongoDB.find (select [] "post")


serverRouting :: Server OurBlogAPI
serverRouting = 
        return "Hello!"
          
        :<|> addNewPost
          
        :<|> addNewPost2
          
        :<|> deletePost
          
        :<|> deletePost2
          
        :<|> getPostById
          
        :<|> getAllPosts

        :<|> do
            pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
            e <- liftIO $ access pipe master "servantAPI" allTeams
            liftIO $ close pipe
            return $ (show e)

        -- :<|> do
        --     pipe <- liftIO $ connect (Database.MongoDB.host "localhost")
        --     e <- liftIO $ access pipe master "servantAPI" (getNextPostId)
        --     liftIO $ close pipe
        --     return $ (show $ valueAt "postId" (fromJust e))
            
            -- manager <- liftIO $ newManager defaultManagerSettings

            -- request <- liftIO $ parseRequest "http://httpbin.org/get"
            -- response <- liftIO $ httpLbs request manager
            -- return $ show $ e

api :: Data.Proxy.Proxy OurBlogAPI
api = Data.Proxy.Proxy

app :: Application
app = simpleCors (serve api serverRouting)

main :: IO ()
main = run 8081 app