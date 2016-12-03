{-# LANGUAGE TupleSections #-}
module Text.Feed.Merge ( merge_feeds ) where

import Control.Monad
import Data.Function
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Time.Locale.Compat
import Data.Time.Format (parseTimeM)
import Data.Time.Clock
import System.IO
import Text.Feed.Constructor
import Text.Feed.Query
import Text.Feed.Types

infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

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

merge_feeds_items :: [Feed] -> IO [(Feed, Item)]
merge_feeds_items feeds = do
  feeds <- return $ map (\feed -> (feed,) <$> feedItems feed) feeds
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

merge_feeds :: [Feed] -> IO Feed
merge_feeds feeds = do
  items <- merge_feeds_items feeds
  items <- return $
    items
    |> map (\(feed, item) -> amend_item_author feed item)
    |> map normalize_item
  feed <- return $
    newFeed (RSSKind Nothing)
    |> withFeedTitle "Merged feed"
    |> withFeedDescription "A merged RSS feed"
    |> withFeedItems items
  return feed
