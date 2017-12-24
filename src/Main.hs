{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import Network.Wreq
import Control.Lens
import Control.Concurrent.Async

import Data.Char
import Data.Int
import Data.Time
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.List(foldl')
import qualified Data.Text as T

data Tree a = Leaf | LeafNode a | StartNodes [Tree a] | Node a [Tree a]

--not currently used
type AppName = ()

data Event = QuitEvent | PreviousItem | NextItem
data AppState = AppState { _AppState_stories :: [Either String HNItem] }

main :: IO AppState
main = do
  stories <- testGetStories
  let app :: App AppState Event AppName
      app = App { appDraw = drawUI 
                  , appHandleEvent = handleEvent
                  , appStartEvent = return 
                  , appAttrMap = const theMap
                  , appChooseCursor = neverShowCursor
                  }
      initialState =  AppState { _AppState_stories = stories }
  defaultMain app initialState

theMap :: AttrMap
theMap = attrMap V.defAttr []

handleEvent :: AppState -> BrickEvent AppName Event -> EventM AppName (Next AppState)
handleEvent state (AppEvent QuitEvent) = halt state
handleEvent state (AppEvent PreviousItem) = continue state
handleEvent state (AppEvent NextItem) = continue state
handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = handleEvent state (AppEvent QuitEvent)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'k') [])) = handleEvent state (AppEvent PreviousItem)
handleEvent state (VtyEvent (V.EvKey V.KUp [])) = handleEvent state (AppEvent PreviousItem)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'j') [])) = handleEvent state (AppEvent NextItem)
handleEvent state (VtyEvent (V.EvKey V.KDown [])) = handleEvent state (AppEvent NextItem)
handleEvent state _ = continue state

drawUI :: AppState -> [Widget AppName]
drawUI state =
    [
    withBorderStyle unicode $
    str "|q| Quit" <=>
    -- commentsTreeView 0 (StartNodes [Node c1 [LeafNode c2], LeafNode c3]) <=>
    storiesView (_AppState_stories state)
    ]

commentView :: Either String HNItem -> Widget AppName
commentView comment =
  let getItemView :: HNItem -> Widget AppName
      getItemView item =
        let author = fromMaybe "N/A" $ _HNItem_by item
            text = fromMaybe "" $ _HNItem_text item
        in
          txt author <=>
          padLeft (Pad 1) (txt text)

      getView :: Either String HNItem -> Widget AppName
      getView item = case item of
        Left error -> border $ str ("Comment could not be loaded: " ++ error)
        Right i -> border $ getItemView i
  in
    getView comment

commentsTreeView :: Int -> Tree (Either String HNItem) -> Widget AppName
commentsTreeView padAmount tree =
  case tree of
    Leaf -> emptyWidget
    LeafNode c -> padLeft (Pad padAmount) $ commentView c
    Node c xs -> let children = map (commentsTreeView (padAmount + 1)) xs
                     widget = padLeft (Pad padAmount) $ commentView c
                     in
                 foldl' (<=>) widget children
    StartNodes xs -> let children = map (commentsTreeView (padAmount)) xs
                        in
                     foldl' (<=>) emptyWidget children

storiesView :: [Either String HNItem] -> Widget AppName
storiesView items =
  let getItemView :: HNItem -> Widget AppName
      getItemView item =
        let author = T.unpack $ fromMaybe "N/A" $ _HNItem_by item
            title = fromMaybe "No title" $ _HNItem_title item
            score = fromMaybe 0 $ _HNItem_score item
            descendants = fromMaybe 0 $ _HNItem_descendants item
            line = (show score) ++ " points by " ++ author ++ " " ++ (show descendants) ++ " comments"
        in
          txt title <=>
          padLeft (Pad 2) (str line)

      getView :: Either String HNItem -> Widget AppName
      getView item = case item of
        Left error -> str ("Story could not be loaded: " ++ error) <=> hBorder
        Right i -> getItemView i <=> hBorder

      views = map getView items 
  in
    hBorder <=> foldl' (<=>) emptyWidget views

-- Data Types

apiItemURL = "https://hacker-news.firebaseio.com/v0/item/"
apiTopStoriesURL = "https://hacker-news.firebaseio.com/v0/topstories.json"
apiBestStoriesURL = "https://hacker-news.firebaseio.com/v0/beststories.json"
apiNewStoriesURL = "https://hacker-news.firebaseio.com/v0/newstories.json"
apiAskStoriesURL = "https://hacker-news.firebaseio.com/v0/askstories.json"
apiShowStoriesURL = "https://hacker-news.firebaseio.com/v0/showstories.json"
apiJobStoriesURL = "https://hacker-news.firebaseio.com/v0/jobstories.json"

data StoriesSortType = SortTop | SortBest | SortNew | SortAsk | SortShow | SortJob

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

type HNID = Int64

data HNType = HNStory | HNComment | HNJob | HNPoll | HNPollopt deriving (Generic, Show)

instance FromJSON HNType where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower . drop 2 }

instance ToJSON HNType where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower . drop 2 }

data HNItem = HNItem {
  
  _HNItem_id :: HNID,
  _HNItem_deleted :: Maybe Bool,
  _HNItem_by :: Maybe Text,
  _HNItem_time :: Int64,
  _HNItem_text :: Maybe Text,
  _HNItem_dead :: Maybe Bool,
  _HNItem_parent :: Maybe HNID,
  _HNItem_poll :: Maybe HNID,
  _HNItem_kids :: Maybe [HNID],
  _HNItem_url :: Maybe Text,
  _HNItem_score :: Maybe Int,
  _HNItem_title :: Maybe Text,
  _HNItem_parts :: Maybe [HNID],
  _HNItem_descendants :: Maybe Int,
  _HNItem_type :: HNType
  
} deriving (Generic, Show)

instance FromJSON HNItem where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 8 }
  
instance ToJSON HNItem where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 8 }

jsonURL = "https://hacker-news.firebaseio.com/v0/item/8863.json"

getJSON url = do
  response <- get url
  let body = response ^. responseBody
  let item = eitherDecode body :: Either String HNItem
  return item

getHNItemKids item = do
  let kidsIds = case _HNItem_kids item of
        Just xs -> xs
        Nothing -> []
  let urls = map getAPIURLForItemFromID kidsIds
  kids <- mapConcurrently getJSON urls
  return kids

kidsTest = do
  item <- getJSON jsonURL
  case item of
    Left _ -> return []
    Right i -> getHNItemKids i

testGetStories = do
  storyIds <- getStoryIds SortTop
  case storyIds of
    Left error -> return [ Left error ]
    Right ids -> do
      let urls = map getAPIURLForItemFromID $ take 20 ids
      items <- mapConcurrently getJSON urls
      return items
