{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Core(vBox, viewport)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V

import GHC.Generics
import System.Process(rawSystem)
import System.Exit
import Data.Aeson
import Data.Aeson.Types

import Network.Wreq
import Control.Lens
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Char
import Data.Int
import Data.Time
import Data.Maybe(fromMaybe, isNothing)
import Data.Text(Text)
import Data.List(foldl', mapAccumL, genericReplicate)
import qualified Data.Text as T
import Text.HTML.TagSoup

data Tree a = Leaf | LeafNode a | StartNodes [Tree a] | Node a [Tree a] deriving (Show)

instance Foldable Tree where
  foldr f z Leaf = z
  foldr f z (LeafNode x) = f x z
  foldr f z (StartNodes []) = z
  foldr f z (StartNodes (x:xs)) = foldr f (foldr f z (StartNodes xs)) x
  foldr f z (Node x []) = f x z
  foldr f z (Node x xs) = f x (foldr f z (StartNodes xs))

data AppName = ViewportHeader | ViewportMain deriving (Ord, Show, Eq)

data Event = HelpEvent | QuitEvent | PreviousItem | NextItem | OpenItem | LoadComments | BackToStories | LoadNextStories | LoadPreviousStories

data AppView = HelpView AppView | StoriesView | CommentsView HNItem (Tree (Either String HNItem))
data AppState = AppState { _AppState_stories :: [Either String HNItem]
                         , _AppState_storyIds :: Either String [HNID]
                         , _AppState_loadedStoryNum :: Int
                         , _AppState_selectedStory :: Int
                         , _AppState_selectedComment :: Int
                         , _AppState_nComments :: Int
                         , _AppState_storiesSort :: StoriesSortType
                         , _AppState_view :: AppView
                         }

main :: IO AppState
main = do
  initialState <- getInitialState
  let app :: App AppState Event AppName
      app = App { appDraw = drawUI 
                  , appHandleEvent = handleEvent
                  , appStartEvent = return
                  , appAttrMap = const theMap
                  , appChooseCursor = neverShowCursor
                  }
  defaultMain app initialState

selectedStoryAttr, selectedCommentAttr, defaultAttr :: AttrName
selectedStoryAttr = "selectedStory"
selectedCommentAttr = "selectedComment"
defaultAttr = "defaultAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (selectedStoryAttr, fg V.red)
  , (selectedCommentAttr, fg V.red)
  , (defaultAttr, V.defAttr)
  ]

handleEvent :: AppState -> BrickEvent AppName Event -> EventM AppName (Next AppState)
handleEvent state (AppEvent QuitEvent) =
  let view = _AppState_view state
    in
  case view of
    HelpView lastView -> continue $ state {_AppState_view = lastView }
    _ -> halt state
handleEvent state (AppEvent HelpEvent) = continue $ handleShowHelpEvent state
handleEvent state (AppEvent PreviousItem) = continue $ handleNextItemEvent state True
handleEvent state (AppEvent NextItem) = continue $ handleNextItemEvent state False
handleEvent state (AppEvent LoadPreviousStories) = liftIO (handleNextStoriesEvent state True) >>= continue 
handleEvent state (AppEvent LoadNextStories) = liftIO (handleNextStoriesEvent state False) >>= continue 
handleEvent state (AppEvent OpenItem) = liftIO (handleOpenItemEvent state) >>= continue
handleEvent state (AppEvent LoadComments) = liftIO (handleLoadCommentsEvent state) >>= continue
handleEvent state (AppEvent BackToStories) = continue $ handleBackToStoriesEvent state
handleEvent state (VtyEvent (V.EvKey (V.KChar 'q') [])) = handleEvent state (AppEvent QuitEvent)
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = handleEvent state (AppEvent QuitEvent)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'k') [])) = handleEvent state (AppEvent PreviousItem)
handleEvent state (VtyEvent (V.EvKey V.KUp [])) = handleEvent state (AppEvent PreviousItem)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'j') [])) = handleEvent state (AppEvent NextItem)
handleEvent state (VtyEvent (V.EvKey V.KDown [])) = handleEvent state (AppEvent NextItem)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'l') [])) = handleEvent state (AppEvent LoadComments)
handleEvent state (VtyEvent (V.EvKey V.KRight [])) = handleEvent state (AppEvent LoadComments)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'h') [])) = handleEvent state (AppEvent BackToStories)
handleEvent state (VtyEvent (V.EvKey V.KLeft [])) = handleEvent state (AppEvent BackToStories)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'o') [])) = handleEvent state (AppEvent OpenItem)
handleEvent state (VtyEvent (V.EvKey (V.KChar '?') [])) = handleEvent state (AppEvent HelpEvent)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'n') [])) = handleEvent state (AppEvent LoadNextStories)
handleEvent state (VtyEvent (V.EvKey (V.KChar 'p') [])) = handleEvent state (AppEvent LoadPreviousStories)
handleEvent state _ = continue state

handleNextStoriesEvent :: AppState -> Bool -> IO AppState
handleNextStoriesEvent state previous = do
  let view = _AppState_view state
      indexDiff = case previous of
        True -> -storiesPerPage
        False -> storiesPerPage
      storyIds =  case (_AppState_storyIds state) of
        Left _ -> []
        Right xs -> xs
      storyStartNum = max 0 $ min (length storyIds) $ (_AppState_loadedStoryNum state) + indexDiff
      storyIdsToGet = take storiesPerPage $ drop storyStartNum storyIds
      urls = map getAPIURLForItemFromID storyIdsToGet
      
  case view of
    StoriesView -> do
      stories <- mapConcurrently getJSON urls
      return state { _AppState_stories = stories, _AppState_loadedStoryNum = storyStartNum }
      
    _ -> return state

handleNextItemEvent :: AppState -> Bool -> AppState
handleNextItemEvent state previous =
  let view = _AppState_view state
      selectedItem = case view of
        StoriesView -> _AppState_selectedStory state
        CommentsView _ _ -> _AppState_selectedComment state
        HelpView _ -> 0
      maxItem = case view of
        StoriesView -> storiesPerPage-1
        CommentsView _ _ -> _AppState_nComments state
        HelpView _ -> 0
      nextSelectedItem = case previous of
        True -> max 0 $ selectedItem - 1
        False -> min maxItem $ selectedItem + 1
      nextState = case view of
        StoriesView -> state { _AppState_selectedStory = nextSelectedItem }
        CommentsView _ _ -> state { _AppState_selectedComment = nextSelectedItem }
        HelpView _ -> state
  in
    nextState

handleOpenItemEvent :: AppState -> IO AppState
handleOpenItemEvent state = do
  let item = getAppViewSelectedItem state
      view = _AppState_view state
  exitCode <- case (view, item) of
        (HelpView _, _) -> return ExitSuccess
        (_, Left error) -> return ExitSuccess
        (_, Right i) -> do
          let url = case (_HNItem_url i) of
                Just u -> T.unpack u
                Nothing -> "https://news.ycombinator.com/item?id=" ++ show (_HNItem_id i)
          rawSystem "xdg-open" [url]
  return state

handleLoadCommentsEvent :: AppState -> IO AppState
handleLoadCommentsEvent state = do
  let index = _AppState_selectedStory state
      item = _AppState_stories state !! index
      view = _AppState_view state
      newState = case item of
        Left error -> return state
        Right i -> do
          tree <- getHNItemKidsTree i
          return state { _AppState_view = CommentsView i tree
                       , _AppState_selectedComment = 0
                       , _AppState_nComments = length tree
                       }
  case view of
    CommentsView _ _ -> return state
    HelpView _ -> return state
    _ -> newState

handleBackToStoriesEvent :: AppState -> AppState
handleBackToStoriesEvent state =
  let view = _AppState_view state
    in
  case view of
    HelpView _ -> state
    _ -> state { _AppState_view = StoriesView }

handleShowHelpEvent :: AppState -> AppState
handleShowHelpEvent state =
  let previousView = _AppState_view state
    in
  case previousView of
    HelpView v -> state { _AppState_view = v }
    _ -> state { _AppState_view = HelpView previousView }

drawUI :: AppState -> [Widget AppName]
drawUI state = [ui]
  where ui = withBorderStyle unicode $
             vBox [ vLimit 1 $ viewport ViewportHeader Vertical $ str "|q| Quit |?| Help"
                  , viewport ViewportMain Vertical $ mainView state
                  ]

mainView :: AppState -> Widget AppName
mainView state =
  let view = _AppState_view state
  in
    case view of
      StoriesView -> storiesView (_AppState_selectedStory state) (_AppState_stories state)
      CommentsView item tree -> commentsView (_AppState_selectedComment state) item tree
      HelpView _ -> helpView

commentsView :: Int -> HNItem -> Tree (Either String HNItem) -> Widget AppName
commentsView selectedIndex item tree =
  let title = fromMaybe "No title" $ _HNItem_title item
      text = fromMaybe "" $ _HNItem_text item
      itemViewBase = hBorder <=>
                     txt title <=>
                     txtWrap text <=>
                     hBorder
      itemView = if selectedIndex == 0 then visible $ withAttr selectedCommentAttr itemViewBase
                 else withAttr defaultAttr itemViewBase
      treeView = case (commentsTreeView 0 selectedIndex 1 tree) of
        (_, v) -> v
    in
  itemView <=>
  treeView

commentView :: Int -> Int -> Either String HNItem -> Widget AppName
commentView selectedIndex index comment =
  let getItemView :: HNItem -> Widget AppName
      getItemView item =
        let author = fromMaybe "N/A" $ _HNItem_by item
            text = fromMaybe "" $ _HNItem_text item
            view = if isNothing (_HNItem_by item) && isNothing (_HNItem_text item) then emptyWidget
                   else txt author <=>
                        padLeft (Pad 1) (txtWrap text)
        in
          view

      getView :: Either String HNItem -> Widget AppName
      getView item = case item of
        Left error -> border $ str ("Comment could not be loaded: " ++ error)
        Right i -> border $ getItemView i
      
      view = getView comment
  in
    if selectedIndex == index then visible $ withAttr selectedCommentAttr view
    else withAttr defaultAttr view

commentsTreeView :: Int -> Int -> Int -> Tree (Either String HNItem) -> (Int, Widget AppName)
commentsTreeView padAmount selectedIndex index tree =
  let accum :: Int -> Int -> [Tree (Either String HNItem)] -> (Int, [Widget AppName])
      accum pad idx nodes =
        let accum2 :: Int -> Tree(Either String HNItem) -> (Int, Widget AppName)
            accum2 i node = commentsTreeView pad selectedIndex i node
        in
          mapAccumL accum2 idx nodes
  in
    case tree of
        Leaf -> (index+1, emptyWidget)
        LeafNode c -> (index+1, padLeft (Pad padAmount) $ commentView selectedIndex index c)
        Node c xs -> let (newIndex, children) = accum (padAmount+2) (index+1) xs
                         widget = padLeft (Pad padAmount) $ commentView selectedIndex index c
                        in
                    (newIndex, foldl' (<=>) widget children)
        StartNodes xs -> let (newIndex, children) = accum padAmount index xs
                            in
                        (newIndex, foldl' (<=>) emptyWidget children)

storiesView :: Int -> [Either String HNItem] -> Widget AppName
storiesView selectedItem items =
  let getItemView :: HNItem -> Widget AppName
      getItemView item =
        let author = T.unpack $ fromMaybe "N/A" $ _HNItem_by item
            title = fromMaybe "No title" $ _HNItem_title item
            score = fromMaybe 0 $ _HNItem_score item
            descendants = fromMaybe 0 $ _HNItem_descendants item
            line = show score ++ " points by " ++ author ++ " " ++ show descendants ++ " comments"
        in
          txt title <=>
          padLeft (Pad 2) (str line)

      getView :: Int -> Either String HNItem -> Widget AppName
      getView itemNum item =
        let view1 = case item of
                Left error -> str ("Story could not be loaded: " ++ error) <=> hBorder
                Right i -> getItemView i <=> hBorder
            view = if itemNum == 0 then hBorder <=> view1
                   else view1
        in
          if selectedItem == itemNum then visible $ withAttr selectedStoryAttr view
          else withAttr defaultAttr view
          

      viewsAccum = mapAccumL (\x y -> (x+1, (getView x y))) 0 items
      views = case viewsAccum of
                (_, xs) -> xs
  in
    foldl' (<=>) emptyWidget views

helpView :: Widget AppName
helpView =
  let controlsList = [("j, ↓", "Next item")
                     ,("k, ↑", "Previous item")
                     ,("q", "Quit")
                     ,("h, ←", "Go back to stories")
                     ,("l, →", "Load the selected story & comments")
                     ,("o", "Open the selected story/comment (by its URL) via xdg-open")
                     ,("?", "Toggle help display")
                     ,("n", "Load next page of stories")
                     ,("p", "Load previous page of stories")
                     ]

      controlsFunc :: (Widget AppName, Widget AppName) -> (Text, Text) -> (Widget AppName, Widget AppName)
      controlsFunc (leftWidget, rightWidget) (leftText, rightText) = (leftWidget <=> txt leftText, rightWidget <=> txt rightText)

      (leftWidget, rightWidget) = foldl' controlsFunc (emptyWidget, emptyWidget) controlsList
      sepMap = map (\x -> txt x) $ genericReplicate (length controlsList) " | "
      sepWidget = foldl' (<=>) emptyWidget sepMap
    in
  borderWithLabel (str "Help") $ (leftWidget <+> sepWidget <+> rightWidget)
  

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
jsonURL2 = "https://hacker-news.firebaseio.com/v0/item/16000895.json"
jsonURL3 = "https://hacker-news.firebaseio.com/v0/item/16024245.json"

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

kidsTest = do
  item <- getJSON jsonURL2
  case item of
    Left _ -> return Leaf
    Right i -> do
      tree <- getHNItemKidsTree i
      print tree
      return tree

storiesPerPage :: Int
storiesPerPage = 30

getInitialState :: IO AppState
getInitialState = do
  storyIds <- getStoryIds SortTop
  items <- do
    case storyIds of
      Left error -> return [ Left error ]
      Right ids -> do
        let urls = map getAPIURLForItemFromID $ take storiesPerPage ids
        i <- mapConcurrently getJSON urls
        return i
        
  let initialState = AppState { _AppState_stories = items
                              , _AppState_storyIds = storyIds
                              , _AppState_loadedStoryNum = 0
                              , _AppState_selectedStory = 0
                              , _AppState_selectedComment = 0
                              , _AppState_nComments = 0
                              , _AppState_storiesSort = SortTop
                              , _AppState_view = StoriesView
                              }
  return initialState

getAppViewSelectedItem :: AppState -> Either String HNItem
getAppViewSelectedItem state =
  let getCommentItem :: Int -> Tree (Either String HNItem) -> Either String HNItem
      getCommentItem index tree =
        let selectFunc :: (Int, Maybe (Either String HNItem)) -> Either String HNItem -> (Int, Maybe (Either String HNItem))
            selectFunc (i, selectedItem) item = if i == index then (i+1, Just item)
                                                else (i+1, selectedItem)
            itemTup = foldl' selectFunc (0, Nothing) tree
          in
        case itemTup of
          (_, Nothing) -> Left "Error couldn't select item"
          (_, Just x) ->  x
      
      view = _AppState_view state
  in
    case view of
     StoriesView -> let index = _AppState_selectedStory state
                      in
                    _AppState_stories state !! index
     CommentsView item comments -> let index = _AppState_selectedComment state
                                     in
                                   if index == 0 then Right item
                                   else getCommentItem (index-1) comments

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

data CommentMarkup = NormalText Text | URLText Text Text deriving (Show)
data CommentMarkupState = NoMarkup | InATag Text deriving (Show)

processComment :: Text -> (CommentMarkupState, [CommentMarkup])
--TODO: make this work better
processComment comment =
  let tags = parseTags comment
  
      processTagsFunc :: (CommentMarkupState, [CommentMarkup]) -> Tag Text -> (CommentMarkupState, [CommentMarkup])
      processTagsFunc (state, list) tag = case (state, tag) of
        (NoMarkup, TagText text) -> (state, list ++ [NormalText text])
        (NoMarkup, TagOpen "a" tags) ->
          let filterForHrefTag (x, _) = (x == "href")
              filtered = filter filterForHrefTag tags
            in
          case filtered of
            [] -> (InATag "no url", list)
            (x:xs) -> case x of
              (_, url) -> (InATag url, list)
            
        (InATag url, TagText text) -> (NoMarkup, list ++ [URLText url text])
        (_, TagOpen tag _) -> (NoMarkup, list)
        (_, TagClose "a") -> (NoMarkup, list)
        (_, TagClose tag) -> (NoMarkup, list)
        (_, _) -> (NoMarkup, list)
        
    in
  foldl' processTagsFunc (NoMarkup, []) tags

drawCommentText :: Text -> Widget AppName
drawCommentText comment =
  let (markupState, markedUpComments) = processComment comment

      convertToWidget :: CommentMarkup -> Widget AppName
      convertToWidget markup = case markup of
        NormalText text -> txtWrap text
        URLText link text -> hyperlink link $ txtWrap text

      widgets = map convertToWidget markedUpComments
    in
  foldl' (<=>) emptyWidget widgets
        
