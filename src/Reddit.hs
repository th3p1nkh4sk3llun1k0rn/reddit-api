{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reddit(Reddit, runReddit, RedditThingy(..), User(..)) where

import Control.Exception
import Control.Monad.State
import Data.Map as M
import Network.HTTP
import Network.HTTP.Cookie
import Text.DeadSimpleJSON
import Text.DeadSimpleJSON.Types

data RedditThingy = Comment | Account | Link | Message | Subreddit | Award | PromoCampaign

data User = User {hasMail :: Bool, name :: String, isFriend :: Bool, created :: Double, modHash :: String, createdUTC :: Double, linkKarma :: Int, commentKarma :: Int, over18 :: Bool, isGold :: Bool, isMod :: Bool, hasVerifiedEmail :: Bool, id :: String, hasModMail :: Bool}

newtype Reddit a = Reddit (StateT (Cookie, User, String) IO a)
                 deriving(Monad, MonadIO)

instance Show RedditThingy where
  show Comment       = "t1"
  show Account       = "t2"
  show Link          = "t3"
  show Message       = "t4"
  show Subreddit     = "t5"
  show Award         = "t6"
  show PromoCampaign = "t8"

login :: String -> String -> IO (String, String)
login username password = do
  rsp <- simpleHTTP (postRequest $ "http://www.reddit.com/api/login?api_type=json&passwd=" ++ password ++ "&rem=false&user=" ++ username) >>= getResponseBody
  case parse rsp of
    Right (JSON (Object prs)) ->
      case M.lookup "json" prs of
        Just (Object json) -> case M.lookup "data" json of
          Just (Object sweetData) -> case (M.lookup "cookie" sweetData, M.lookup "modhash" sweetData) of
            (Just (String cookie), Just (String modhash)) -> return (cookie, modhash)
            _                           -> undefined
          _                       -> undefined
        _                  -> undefined
    Left err  -> do
      undefined
  
runReddit :: String -> String -> Reddit a -> IO a
runReddit username password (Reddit m) = do
  (c, _) <- login username password
  let cookie = MkCookie{ckDomain = "http:\\www.reddit.com", ckName = "reddit login cookie", ckValue = c, ckPath = Nothing, ckComment = Nothing, ckVersion = Nothing}
  user <- meIO cookie
  auth <- oauth
  evalStateT m (cookie, user, auth)

meIO :: Cookie -> IO User
me c = undefined

oauth :: IO String
oauth = undefined

  {- do
  let s = "http://www.reddit.com/api/me.json"
  rsp <- simpleHTTP (getRequest s) >>= getResponseBody
  return rsp
-}
