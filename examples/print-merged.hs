{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
import Data.Maybe
import Network.Curl
import System.IO
import Text.Feed.Export
import Text.Feed.Import
import Text.Feed.Merge
import Text.Feed.Types
import Text.XML.Light.Output

feed_uris :: [String]
feed_uris =
  [ "http://xkcd.com/rss.xml"
  , "https://blog.acolyer.org/feed/"
  , "http://feeds.feedburner.com/ProceduralWorld"
  ]

fetch_url :: String -> IO String
fetch_url url = do
  (_code, response) <- curlGetString url [CurlFollowLocation True]
  return response

get_feed :: String -> IO (Maybe Feed)
get_feed url = do
  hPutStrLn stderr ("Getting " ++ url)
  feed <- fetch_url url
  parsed <- return $ parseFeedString feed 
  case parsed of
    Just _feed -> return ()
    Nothing -> hPutStrLn stderr ("Error parsing " ++ url)
  return parsed

merged_feed :: IO Feed
merged_feed = do
  hPutStrLn stderr "Fetching feeds.."
  feeds <- mapM get_feed feed_uris
  feeds <- return $ catMaybes feeds
  hPutStrLn stderr "Merging feeds.."
  feed <- merge_feeds feeds
  return feed

main :: IO ()
main = do
  feed <- merged_feed 
  hPutStrLn stderr "Formatting feed.."
  putStrLn (ppElement (xmlFeed feed))
  return ()
