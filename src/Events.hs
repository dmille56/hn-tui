{-# LANGUAGE OverloadedStrings #-}

module Events where

import DataTypes
import NetworkRequests

import Brick
import Control.Concurrent.Async
import Data.List(foldl', mapAccumL, genericReplicate)
import Control.Monad.IO.Class
import System.Process(rawSystem)
import System.Exit
import qualified Graphics.Vty as V
import qualified Data.Text as T

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
handleEvent state (AppEvent (LoadStories sort)) = liftIO (handleLoadStoriesEvent state sort) >>= continue 
handleEvent state (AppEvent OpenItem) = liftIO (handleOpenItemEvent state) >>= continue
handleEvent state (AppEvent LoadComments) = liftIO (handleLoadCommentsEvent state) >>= continue
handleEvent state (AppEvent RefreshEvent) = liftIO (handleRefreshEvent state) >>= continue
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
handleEvent state (VtyEvent (V.EvKey (V.KChar '1') [])) = handleEvent state (AppEvent (LoadStories SortTop))
handleEvent state (VtyEvent (V.EvKey (V.KChar '2') [])) = handleEvent state (AppEvent (LoadStories SortBest))
handleEvent state (VtyEvent (V.EvKey (V.KChar '3') [])) = handleEvent state (AppEvent (LoadStories SortNew))
handleEvent state (VtyEvent (V.EvKey (V.KChar '4') [])) = handleEvent state (AppEvent (LoadStories SortAsk))
handleEvent state (VtyEvent (V.EvKey (V.KChar '5') [])) = handleEvent state (AppEvent (LoadStories SortShow))
handleEvent state (VtyEvent (V.EvKey (V.KChar '6') [])) = handleEvent state (AppEvent (LoadStories SortJob))
handleEvent state (VtyEvent (V.EvKey (V.KFun 5) [])) = handleEvent state (AppEvent RefreshEvent)
handleEvent state _ = continue state

handleRefreshEvent :: AppState -> IO AppState
handleRefreshEvent state =
  let view = _AppState_view state
      sort = _AppState_storiesSort state
    in
  case view of
    HelpView _ -> return state
    StoriesView -> handleLoadStoriesEvent state sort
    CommentsView _ _ -> handleLoadCommentsEvent $ state { _AppState_view = StoriesView }
    
handleLoadStoriesEvent :: AppState -> StoriesSortType -> IO AppState
handleLoadStoriesEvent state sort = 
  let view = _AppState_view state
    in
  case view of
    StoriesView -> do
      storyIds <- getStoryIds sort
      items <- do
        case storyIds of
          Left error -> return [ Left error ]
          Right ids -> do
            let urls = map getAPIURLForItemFromID $ take storiesPerPage ids
            i <- mapConcurrently getJSON urls
            return i

      return state { _AppState_storyIds = storyIds
                   , _AppState_stories = items 
                   , _AppState_loadedStoryNum = 0
                   , _AppState_selectedStory = 0
                   , _AppState_storiesSort = sort
                   , _AppState_nStories = length (items)
                   }

    _ -> return state

handleNextStoriesEvent :: AppState -> Bool -> IO AppState
handleNextStoriesEvent state previous = do
  let view = _AppState_view state
      indexDiff = case previous of
        True -> -storiesPerPage
        False -> storiesPerPage
      storyIds =  case (_AppState_storyIds state) of
        Left _ -> []
        Right xs -> xs
      storyStartNum = max 0 $ min ((length storyIds) - storiesPerPage) $ (_AppState_loadedStoryNum state) + indexDiff
      storyIdsToGet = take storiesPerPage $ drop storyStartNum storyIds
      urls = map getAPIURLForItemFromID storyIdsToGet
      
  case view of
    StoriesView -> do
      stories <- mapConcurrently getJSON urls
      return state { _AppState_stories = stories
                   , _AppState_loadedStoryNum = storyStartNum
                   , _AppState_nStories = length (stories)
                   }
      
    _ -> return state
    
handleNextItemEvent :: AppState -> Bool -> AppState
handleNextItemEvent state previous =
  let view = _AppState_view state
      selectedItem = case view of
        StoriesView -> _AppState_selectedStory state
        CommentsView _ _ -> _AppState_selectedComment state
        HelpView _ -> 0
      maxItem = case view of
        StoriesView -> (_AppState_nStories state) - 1
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
