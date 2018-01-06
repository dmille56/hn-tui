{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module DataTypes where

import Data.Int
import Data.Char(toLower)
import Data.Text(Text)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data Tree a = Leaf | LeafNode a | StartNodes [Tree a] | Node a [Tree a] deriving (Show)

instance Foldable Tree where
  foldr f z Leaf = z
  foldr f z (LeafNode x) = f x z
  foldr f z (StartNodes []) = z
  foldr f z (StartNodes (x:xs)) = foldr f (foldr f z (StartNodes xs)) x
  foldr f z (Node x []) = f x z
  foldr f z (Node x xs) = f x (foldr f z (StartNodes xs))

data AppName = ViewportHeader | ViewportMain deriving (Ord, Show, Eq)

data Event = HelpEvent | QuitEvent | PreviousItem | NextItem | OpenItem | LoadComments | BackToStories | LoadNextStories | LoadPreviousStories | LoadStories StoriesSortType | RefreshEvent

data AppView = HelpView AppView | StoriesView | CommentsView HNItem (Tree (Either String HNItem))

data AppState = AppState { _AppState_stories :: [Either String HNItem]
                         , _AppState_storyIds :: Either String [HNID]
                         , _AppState_loadedStoryNum :: Int
                         , _AppState_selectedStory :: Int
                         , _AppState_selectedComment :: Int
                         , _AppState_nComments :: Int
                         , _AppState_nStories :: Int
                         , _AppState_storiesSort :: StoriesSortType
                         , _AppState_view :: AppView
                         }
                
data StoriesSortType = SortTop | SortBest | SortNew | SortAsk | SortShow | SortJob

type HNID = Integer

type HNTime = Integer

data HNType = HNStory | HNComment | HNJob | HNPoll | HNPollopt deriving (Generic, Show)

instance FromJSON HNType where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map toLower . drop 2 }

instance ToJSON HNType where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower . drop 2 }

data HNItem = HNItem {
  
  _HNItem_id :: HNID,
  _HNItem_deleted :: Maybe Bool,
  _HNItem_by :: Maybe Text,
  _HNItem_time :: HNTime,
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

storiesPerPage :: Int
storiesPerPage = 30
