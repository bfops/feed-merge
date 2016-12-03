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
import System.IO
import Text.Feed.Constructor
import Text.Feed.Export
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.XML.Light.Output

infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

feeds :: String -> IO [String]
feeds file = read <$> readFile file

fetch_url :: String -> IO String
fetch_url url = do
  (_code, response) <- curlGetString url [CurlFollowLocation True]
  return response

get_feed :: String -> IO [(Feed, Item)]
get_feed url = do
  hPutStrLn stderr ("Getting " ++ url)
  feed <- fetch_url url
  case parseFeedString feed of
    Nothing -> do
      hPutStrLn stderr ("Error parsing " ++ url)
      return []
    Just feed -> do
      return ((feed,) <$> feedItems feed)

sortByCompareM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [a]
sortByCompareM comparable l = do
  comparable <- mapM comparable l
  return $ map snd $ sortBy (compare `on` fst) (zip comparable l)

merge :: [[(Feed, Item)]] -> IO [(Feed, Item)]
merge feeds = do
    feeds <- return $ mapMaybe uncons feeds
    feeds <- reverse <$> sortByCompareM last_update feeds
    case feeds of
      [] -> return []
      ((x, feed):feeds) -> do
        feeds <- return $ map (\(x, feed) -> x:feed) feeds
        merged <- merge (feed : feeds)
        return (x : merged)
  where
    last_update :: ((Feed, Item), [(Feed, Item)]) -> IO (Maybe UTCTime)
    last_update ((_feed, item), _rest) = do
      case getItemPublishDateString item of
        Nothing -> do
          hPutStrLn stderr ("Could not find time for " ++ show item)
          return Nothing
        Just as_str -> do
          case parse_published_time as_str of
            Just x -> return (Just x)
            Nothing -> do
              hPutStrLn stderr ("Could not parse publish time " ++ as_str)
              return Nothing

    parse_published_time :: String -> Maybe UTCTime
    parse_published_time s = runIdentity $ do
      let formats =
            [ iso8601DateFormat (Just "%H:%M:%S%Z")
            , iso8601DateFormat (Just "%H:%M:%S%Q%Z")
            , iso8601DateFormat Nothing
            , rfc822DateFormat
            ]
      return $ foldl mplus Nothing (map (\fmt -> parseTimeM True defaultTimeLocale fmt s) formats)

merged_feed_items :: String -> IO [(Feed, Item)]
merged_feed_items feeds_file = do
  feeds <- feeds feeds_file
  feeds <- mapM get_feed feeds
  hPutStrLn stderr "Finished fetching"
  merged <- merge feeds
  return merged

-- Add feed info to the author of the item
amend_item_author :: Feed -> Item -> Item
amend_item_author feed item = runIdentity $ do
  feed_title <- return $ getFeedTitle feed
  author <-
    case getItemAuthor item of
      Nothing -> return feed_title
      Just author -> return (feed_title ++ ": " ++ author)
  return $ withItemAuthor author item

infixl 3 <?>
(<?>) :: Maybe a -> a -> a
Nothing <?> x = x
Just x <?> _ = x

-- Convert an item to a common type
normalize_item :: Item -> Item
normalize_item item =
  newItem (RSSKind Nothing)
  |> (withItemTitle <$> getItemTitle item <?> id)
  |> (withItemLink <$> getItemLink item <?> id)
  |> (withItemPubDate <$> getItemPublishDateString item <?> id)
  |> (withItemAuthor <$> getItemAuthor item <?> id)
  |> (withItemDescription <$> getItemDescription item <?> id)

merged_feed :: String -> IO Feed
merged_feed feeds_file = do
  items <- merged_feed_items feeds_file
  items <- return $
    items
    |> map (\(feed, item) -> amend_item_author feed item)
    |> map normalize_item
  feed <- return $
    newFeed (RSSKind Nothing)
    |> withFeedTitle "Merged feed"
    |> withFeedItems items
  return feed

main :: IO ()
main = do
  args <- getArgs
  case args of
    [feeds_file] -> do
      feed <- merged_feed feeds_file
      putStrLn (ppElement (xmlFeed feed))
      return ()
    _ -> do
      hPutStrLn stderr "Unexpected arguments"
      return ()
