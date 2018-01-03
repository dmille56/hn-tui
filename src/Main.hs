module Main where

import DataTypes(AppState(..), StoriesSortType(..), Event(..), AppName(..), AppView(..), storiesPerPage)
import Events(handleEvent)
import NetworkRequests(getStoryIds, getAPIURLForItemFromID, getJSON)
import View(drawUI, theMap)

import Brick
import Control.Concurrent.Async

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
                              , _AppState_nStories = length (items)
                              , _AppState_storiesSort = SortTop
                              , _AppState_view = StoriesView
                              }
  return initialState
