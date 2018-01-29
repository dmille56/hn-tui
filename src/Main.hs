module Main where

import DataTypes(AppState(..), StoriesSortType(..), Event(..), AppName(..), AppView(..), storiesPerPage)
import Events(handleEvent)
import NetworkRequests
import View(drawUI, defaultTheme)

import Brick
import Brick.Themes (loadCustomizations, themeToAttrMap)
import qualified Graphics.Vty as V
import Control.Concurrent.Async
import Data.Time
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      _ <- runApp $ themeToAttrMap defaultTheme
      return ()
    ["--theme", fileName] -> runWithTheme fileName
    _ -> putStrLn "Unknown arguments passed in, doing nothing"

runWithTheme :: String -> IO ()
runWithTheme themeFileName = do
  customizedTheme <- loadCustomizations themeFileName defaultTheme
  case customizedTheme of
    Left x -> putStrLn $ "failed to load theme: '" ++ themeFileName ++ "'"
    Right theme -> do
      _ <- runApp $ themeToAttrMap theme
      return ()

runApp :: AttrMap -> IO AppState
runApp myMap = do
  initialState <- getInitialState
  let app :: App AppState Event AppName
      app = App { appDraw = drawUI 
                  , appHandleEvent = handleEvent
                  , appStartEvent = return
                  , appAttrMap = const myMap 
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
        i <- mapConcurrently getJSON $ take storiesPerPage ids
        return i

  time <- getCurrentTime
  
  let initialState = AppState { _AppState_stories = items
                              , _AppState_storyIds = storyIds
                              , _AppState_loadedStoryNum = 0
                              , _AppState_selectedStory = 0
                              , _AppState_selectedComment = 0
                              , _AppState_nComments = 0
                              , _AppState_nStories = length (items)
                              , _AppState_storiesSort = SortTop
                              , _AppState_view = StoriesView
                              , _AppState_time = time
                              }
  return initialState
