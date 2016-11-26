{-# LANGUAGE TupleSections #-}
import Control.Monad
import Data.Function
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Time.Locale.Compat
import Data.Time.Format (parseTimeM)
import Data.Time.Clock
import Network.Curl
import System.Environment
import Text.Feed.Constructor
import Text.Feed.Import
import Text.Feed.Export
import Text.Feed.Query
import Text.Feed.Types

feeds :: String -> IO [String]
feeds file = read <$> readFile file

fetch_url :: String -> IO String
fetch_url url = do
  (_code, response) <- curlGetString url [CurlFollowLocation True]
  return response

get_feed :: String -> IO [(Feed, Item)]
get_feed url = do
  print ("Getting " ++ url)
  feed <- fetch_url url
  case parseFeedString feed of
    Nothing -> do
      print ("Error parsing " ++ url)
      return []
    Just feed -> do
      return ((feed,) <$> feedItems feed)

merge :: [[(Feed, Item)]] -> [(Feed, Item)]
merge feeds = runIdentity $ do
    feeds <- return $ mapMaybe uncons feeds
    feeds <- return $ sortBy (\x y -> negate_ord $ (compare `on` last_update) x y) feeds
    case feeds of
      [] -> return []
      ((x, feed):feeds) -> do
        feeds <- return $ map (\(x, feed) -> x:feed) feeds
        return $ x : merge (feed : feeds)
  where
    negate_ord EQ = EQ
    negate_ord GT = LT
    negate_ord LT = GT

    last_update :: ((Feed, Item), [(Feed, Item)]) -> UTCTime
    last_update ((_feed, item), _rest) = runIdentity $ do
      let as_str = fromJust $ getItemPublishDateString item
      case parse_published_time as_str of
        Nothing -> error ("Could not parse publish time " ++ as_str)
        Just x -> return x

    parse_published_time :: String -> Maybe UTCTime
    parse_published_time s = runIdentity $ do
      let formats =
            [ iso8601DateFormat (Just "%H:%M:%S%Z")
            , iso8601DateFormat (Just "%H:%M:%S%Q%Z")
            , iso8601DateFormat Nothing
            , rfc822DateFormat
            ]
      return $ foldl mplus Nothing (map (\fmt -> parseTimeM True defaultTimeLocale fmt s) formats)

merged_feed :: String -> IO [(Feed, Item)]
merged_feed feeds_file = do
  feeds <- feeds feeds_file
  feeds <- mapM get_feed feeds
  print "Finished fetching"
  return $ merge feeds

main :: IO ()
main = do
  args <- getArgs
  case args of
    [feeds_file] -> do
      feed <- merged_feed feeds_file
      feed <- return $ runIdentity $ do
        feed <- return $ snd <$> feed
        feed <- return $ withFeedItems feed (newFeed (RSSKind Nothing))
        feed <- return $ xmlFeed feed
        return feed
      print feed
      return ()
    _ -> do
      print "Unexpected arguments"
      return ()
