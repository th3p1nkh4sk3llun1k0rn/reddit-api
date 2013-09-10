module Reddit where

import Network.Curl
import Text.JSON

data RedditThingy = Comment | Account | Link | Message | Subreddit | Award | PromoCampaign

instance Show RedditThingy where
  show Comment       = "t1"
  show Account       = "t2"
  show Link          = "t3"
  show Message       = "t4"
  show Subreddit     = "t5"
  show Award         = "t6"
  show PromoCampaign = "t8"
