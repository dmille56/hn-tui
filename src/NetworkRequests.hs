{-# LANGUAGE OverloadedStrings #-}

module NetworkRequests where

import DataTypes

import Data.Text(Text)
import Data.List(foldl')
import qualified Data.Text as T
import Data.Aeson
import Network.Wreq
import Control.Lens
import Control.Concurrent.Async

apiItemURL = "https://hacker-news.firebaseio.com/v0/item/"
apiTopStoriesURL = "https://hacker-news.firebaseio.com/v0/topstories.json"
apiBestStoriesURL = "https://hacker-news.firebaseio.com/v0/beststories.json"
apiNewStoriesURL = "https://hacker-news.firebaseio.com/v0/newstories.json"
apiAskStoriesURL = "https://hacker-news.firebaseio.com/v0/askstories.json"
apiShowStoriesURL = "https://hacker-news.firebaseio.com/v0/showstories.json"
apiJobStoriesURL = "https://hacker-news.firebaseio.com/v0/jobstories.json"

getStoryIds :: StoriesSortType -> IO (Either String [HNID])
getStoryIds sortType = let url = case sortType of
                             SortTop -> apiTopStoriesURL
                             SortBest -> apiBestStoriesURL
                             SortNew -> apiNewStoriesURL
                             SortAsk -> apiAskStoriesURL
                             SortShow -> apiShowStoriesURL
                             SortJob -> apiJobStoriesURL
                        in
                       do
                         response <- get url
                         let body = response ^. responseBody
                         return (eitherDecode body :: Either String [HNID])

getAPIURLForItemFromID :: HNID -> String
getAPIURLForItemFromID id = apiItemURL ++ (show id) ++ ".json"

getJSON url = do
  response <- get url
  let body = response ^. responseBody
  let item = eitherDecode body :: Either String HNItem
  let itemReplaceText = case item of
        Left _ -> item
        Right i -> case (_HNItem_text i) of
                     Just x -> Right $ i { _HNItem_text = Just (replaceSequencesText x) }
                     Nothing -> item
  return itemReplaceText

getHNItemKids :: HNItem -> IO [Either String HNItem]
getHNItemKids item = do
  let kidsIds = case _HNItem_kids item of
        Just xs -> xs
        Nothing -> []
  let urls = map getAPIURLForItemFromID kidsIds
  kids <- mapConcurrently getJSON urls
  return kids

isCommentValid :: Either String HNItem -> Bool
isCommentValid (Left e) = True
isCommentValid (Right item) =
  let text = _HNItem_text item
      author = _HNItem_by item
      itemType = _HNItem_type item
    in
  case (text, author, itemType) of
    (Nothing, Nothing, HNComment) -> False
    (_, _, _) -> True
  
getHNItemKidsRecursive :: HNID -> IO (Tree (Either String HNItem))
getHNItemKidsRecursive id = do
  let url = getAPIURLForItemFromID id
  item <- getJSON url
  let valid = isCommentValid item
  case (valid, item) of
    (False, _) -> return $ Leaf
    (_, Left _) -> return $ LeafNode item
    (_, Right i) -> case _HNItem_kids i of
      Nothing -> return $ LeafNode item
      Just [] -> return $ LeafNode item
      Just kidIds -> do
        tree <- mapConcurrently getHNItemKidsRecursive kidIds
        return $ Node item tree

getHNItemKidsTree :: HNItem -> IO (Tree (Either String HNItem))
getHNItemKidsTree item = do
  let getItemKids :: Either String HNItem -> IO (Tree (Either String HNItem))
      getItemKids it =
        let valid = isCommentValid it
          in
        case (valid, it) of
          (False, _) -> return $ Leaf
          (_, Left _) -> return $ LeafNode it
          (_, Right i) -> case _HNItem_kids i of
            Nothing -> return $ LeafNode it
            Just [] -> return $ LeafNode it
            Just kidIds -> do
                tree <- mapConcurrently getHNItemKidsRecursive kidIds
                return $ Node it tree
  
  let kidsIds = case _HNItem_kids item of
        Just xs -> xs
        Nothing -> []
  let urls = map getAPIURLForItemFromID kidsIds
  kids <- mapConcurrently getJSON urls
  let filteredKids = filter isCommentValid kids
  startNodes <- mapM getItemKids filteredKids
  
  case startNodes of
    [] -> return Leaf
    _ -> return $ StartNodes startNodes

replaceSequencesText :: Text -> Text
--replace character sequences with the actual characters
replaceSequencesText text =
  let replaceFunc :: Text -> (Text, Text) -> Text
      replaceFunc input (search, replace) = T.replace search replace input
      
      replaceTextList :: [(Text, Text)]
      replaceTextList = [ ("&#x2F;", "/")
                        , ("&#x27;", "'")
                        , ("&quot;", "\"")
                        , ("&gt;", ">")
                        , ("&lt;", "<")
                        , ("&amp;", "&")
                        , ("<p>", "\n\n")
                        , ("</p>", "")
                        ]
    in
  foldl' replaceFunc text replaceTextList
