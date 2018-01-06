{-# LANGUAGE OverloadedStrings #-}

module NetworkRequests where

import DataTypes

import Data.Text(Text)
import Data.List(foldl')
import qualified Data.Text as T
import Data.Aeson
import Control.Exception (throwIO)
import Control.Concurrent.Async
import qualified Network.HTTP.Req as R
import Control.Monad.IO.Class

apiBaseURL, apiVersion, apiSortTop, apiSortBest, apiSortNew, apiSortAsk, apiSortShow, apiSortJob, apiItem :: Text
apiBaseURL = "hacker-news.firebaseio.com"
apiVersion = "v0"
apiSortTop = "topstories.json"
apiSortBest = "beststories.json"
apiSortNew = "newstories.json"
apiSortAsk = "askstories.json"
apiSortShow = "showstories.json"
apiSortJob = "jobstories.json"
apiItem = "item"

--TODO: maybe handle http exceptions in the future?
instance R.MonadHttp IO where
  handleHttpException = throwIO

getStoryIds :: StoriesSortType -> IO (Either String [HNID])
getStoryIds sortType = let apiText :: Text
                           apiText = case sortType of
                              SortTop -> apiSortTop
                              SortBest -> apiSortBest
                              SortNew -> apiSortNew
                              SortAsk -> apiSortAsk
                              SortShow -> apiSortShow
                              SortJob -> apiSortJob 
                         in do
  r <- R.req R.GET
    (R.https apiBaseURL R./: apiVersion R./: apiText) R.NoReqBody R.bsResponse mempty
    
  let maybe :: Maybe [HNID]
      maybe = decodeStrict' (R.responseBody r) :: Maybe [HNID]

      result :: Either String [HNID]
      result = case maybe of
                 Just x -> Right x
                 Nothing -> Left "Error couldn't deserialize json into StoryItems"
  return result

getJSON :: HNID -> IO (Either String HNItem)
getJSON id = do
  r <- R.req R.GET
    (R.https apiBaseURL R./: apiVersion R./: apiItem R./: T.pack ((show id) ++ ".json")) R.NoReqBody R.bsResponse mempty
    
  let maybe :: Maybe HNItem
      maybe = decodeStrict' (R.responseBody r) :: Maybe HNItem

      result :: Either String HNItem
      result = case maybe of
                 Just x -> case (_HNItem_text x) of
                   Just i -> Right $ x { _HNItem_text = Just (replaceSequencesText i) }
                   Nothing -> Right x
                   
                 Nothing -> Left "Error couldn't deserialize json into HNItem"

  return result

getHNItemKids :: HNItem -> IO [Either String HNItem]
getHNItemKids item = do
  let kidsIds :: [HNID]
      kidsIds = case _HNItem_kids item of
        Just xs -> xs
        Nothing -> []
      
  liftIO $ mapConcurrently getJSON kidsIds

isCommentValid :: Either String HNItem -> Bool
isCommentValid (Left e) = True
isCommentValid (Right item) =
  let text :: Maybe Text
      text = _HNItem_text item

      author :: Maybe Text
      author = _HNItem_by item

      itemType :: HNType 
      itemType = _HNItem_type item
    in
  case (text, author, itemType) of
    (Nothing, Nothing, HNComment) -> False
    (_, _, _) -> True
  
getHNItemKidsRecursive :: HNID -> IO (Tree (Either String HNItem))
getHNItemKidsRecursive id = do
  item <- getJSON id
  let valid :: Bool
      valid = isCommentValid item
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
        let valid :: Bool
            valid = isCommentValid it
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
  
  let kidsIds :: [HNID]
      kidsIds = case _HNItem_kids item of
        Just xs -> xs
        Nothing -> []
  kids <- mapConcurrently getJSON kidsIds
  let filteredKids :: [Either String HNItem]
      filteredKids = filter isCommentValid kids
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
