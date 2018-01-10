{-# LANGUAGE OverloadedStrings #-}

module View where

import DataTypes

import Brick
import Brick.Widgets.Core(vBox, viewport)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Data.Maybe(fromMaybe, isNothing)
import Data.Text(Text)
import Data.List(foldl', mapAccumL, genericReplicate)
import Data.Time
import Data.Time.Clock.POSIX

import Text.HTML.TagSoup
import qualified Graphics.Vty as V
import qualified Data.Text as T

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

drawUI :: AppState -> [Widget AppName]
drawUI state = [ui]
  where ui = withBorderStyle unicode $
             vBox [ vLimit 1 $ viewport ViewportHeader Vertical $ str "|q| Quit |?| Help"
                  , viewport ViewportMain Vertical $ mainView state
                  ]

mainView :: AppState -> Widget AppName
mainView state =
  let view = _AppState_view state
      time = _AppState_time state
  in
    case view of
      StoriesView -> storiesView time (_AppState_selectedStory state) (_AppState_stories state)
      CommentsView item tree -> commentsView time (_AppState_selectedComment state) item tree
      HelpView _ -> helpView

commentsView :: UTCTime -> Int -> HNItem -> Tree (Either String HNItem) -> Widget AppName
commentsView time selectedIndex item tree =
  let title = fromMaybe "No title" $ _HNItem_title item
      text = fromMaybe "" $ _HNItem_text item
      author = T.unpack $ fromMaybe "N/A" $ _HNItem_by item
      score = fromMaybe 0 $ _HNItem_score item
      descendants = fromMaybe 0 $ _HNItem_descendants item
      storyTime = posixSecondsToUTCTime $ _HNItem_time item
      timeDiff = diffUTCTime time storyTime
      line = show score ++ " points by " ++ author ++ " " ++ showNominalDiffTime timeDiff ++ " " ++ show descendants ++ " comments"
      itemViewBase = hBorder <=>
                     txt title <=>
                     txtWrap text <=>
                     str line <=>
                     hBorder
      itemView = if selectedIndex == 0 then visible $ withAttr selectedCommentAttr itemViewBase
                 else withAttr defaultAttr itemViewBase
      treeView = case (commentsTreeView time 0 selectedIndex 1 tree) of
        (_, v) -> v
    in
  itemView <=>
  treeView

commentView :: UTCTime -> Int -> Int -> Either String HNItem -> Widget AppName
commentView time selectedIndex index comment =
  let getItemView :: HNItem -> Widget AppName
      getItemView item =
        let author = T.unpack $ fromMaybe "N/A" $ _HNItem_by item
            text = fromMaybe "" $ _HNItem_text item
            commentTime = posixSecondsToUTCTime $ _HNItem_time item
            timeDiff = diffUTCTime time commentTime
            line = author ++ " " ++ showNominalDiffTime timeDiff
            view = if isNothing (_HNItem_by item) && isNothing (_HNItem_text item) then emptyWidget
                   else str line <=>
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

commentsTreeView :: UTCTime -> Int -> Int -> Int -> Tree (Either String HNItem) -> (Int, Widget AppName)
commentsTreeView time padAmount selectedIndex index tree =
  let accum :: Int -> Int -> [Tree (Either String HNItem)] -> (Int, [Widget AppName])
      accum pad idx nodes =
        let accum2 :: Int -> Tree(Either String HNItem) -> (Int, Widget AppName)
            accum2 i node = commentsTreeView time pad selectedIndex i node
        in
          mapAccumL accum2 idx nodes
  in
    case tree of
        Leaf -> (index+1, emptyWidget)
        LeafNode c -> (index+1, padLeft (Pad padAmount) $ commentView time selectedIndex index c)
        Node c xs -> let (newIndex, children) = accum (padAmount+2) (index+1) xs
                         widget = padLeft (Pad padAmount) $ commentView time selectedIndex index c
                        in
                    (newIndex, foldl' (<=>) widget children)
        StartNodes xs -> let (newIndex, children) = accum padAmount index xs
                            in
                        (newIndex, foldl' (<=>) emptyWidget children)

storiesView :: UTCTime -> Int -> [Either String HNItem] -> Widget AppName
storiesView time selectedItem items =
  let getItemView :: HNItem -> Widget AppName
      getItemView item =
        let author = T.unpack $ fromMaybe "N/A" $ _HNItem_by item
            title = fromMaybe "No title" $ _HNItem_title item
            score = fromMaybe 0 $ _HNItem_score item
            descendants = fromMaybe 0 $ _HNItem_descendants item
            storyTime = posixSecondsToUTCTime $ _HNItem_time item
            timeDiff = diffUTCTime time storyTime
            line = show score ++ " points by " ++ author ++ " " ++ showNominalDiffTime timeDiff ++ " " ++ show descendants ++ " comments"
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
                     ,("1", "Load stories sorted by top")
                     ,("2", "Load stories sorted by best")
                     ,("3", "Load stories sorted by new")
                     ,("4", "Load ask stories ")
                     ,("5", "Load show stories")
                     ,("6", "Load job stories")
                     ,("F5, r", "Refresh the current view")
                     ]

      controlsFunc :: (Widget AppName, Widget AppName) -> (Text, Text) -> (Widget AppName, Widget AppName)
      controlsFunc (leftWidget, rightWidget) (leftText, rightText) = (leftWidget <=> txt leftText, rightWidget <=> txt rightText)

      (leftWidget, rightWidget) = foldl' controlsFunc (emptyWidget, emptyWidget) controlsList
      sepMap = map (\x -> txt x) $ genericReplicate (length controlsList) " | "
      sepWidget = foldl' (<=>) emptyWidget sepMap
    in
  borderWithLabel (str "Help") $ (leftWidget <+> sepWidget <+> rightWidget)

  
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

showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime diff =
  let seconds = max 0 $ floor diff
      minutes = seconds `div` 60
      hours = minutes `div` 60
      days = hours `div` 24

      showFunc :: Int -> Int -> Int -> Int -> String
      showFunc secs mins hrs dys
        | secs == 1 = "1 second ago"
        | secs < 60 = show secs ++ " seconds ago"
        | mins == 1 = "1 minute ago"
        | mins < 60 = show mins ++ " minutes ago"
        | hrs == 1 = "1 hour ago"
        | hrs < 24 = show hrs ++ " hours ago"
        | dys == 1 = "1 day ago"
        | otherwise = show days ++ " days ago"

    in
  showFunc seconds minutes hours days
