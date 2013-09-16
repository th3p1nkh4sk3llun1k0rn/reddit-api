{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reddit where

import Control.Monad.State
import Network.HTTP
import Text.JSON

data RedditThingy = Comment | Account | Link | Message | Subreddit | Award | PromoCampaign

data User = User {hasMail :: Bool, name :: String, isFriend :: Bool, created :: Double, modHash :: String, createdUTC :: Double, linkKarma :: Int, commentKarma :: Int, over18 :: Bool, isGold :: Bool, isMod :: Bool, hasVerifiedEmail :: Bool, id :: String, hasModMail :: Bool}

newtype Reddit a = Reddit (StateT (User, String) IO a)
                 deriving(Monad, MonadIO)

instance Show RedditThingy where
  show Comment       = "t1"
  show Account       = "t2"
  show Link          = "t3"
  show Message       = "t4"
  show Subreddit     = "t5"
  show Award         = "t6"
  show PromoCampaign = "t8"

login :: String -> String -> IO String
login username password = simpleHTTP (postRequest $ "http://www.reddit.com/api/login?api_type=json&passwd=" ++ password ++ "&rem=false&user=" ++ username) >>= getResponseBody

{-me :: IO User
me = do
  let s = "http://www.reddit.com/api/me.json"
  case parseURI s of
    Nothing -> undefined
    Just uri -> do
      tryGet <- httpGet uri
      case tryGet of
        Nothing -> undefined
        Just content -> print content
  undefined-}
