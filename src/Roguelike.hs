{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Roguelike (
    roguelike
  ) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent (threadDelay)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)

import Reflex
import Reflex.Brick

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Control.Lens hiding (Empty)
import Linear.V2 (V2(..), _x, _y)

type Name = ()

type Coord = V2 Int

data Terrain =
    Floor
  | Wall

data OutputState =
  OutputState {
    playerPos :: Coord
  , gameMap :: Map Coord Terrain
  }

drawWithCentre :: (Int -> Int -> Widget n) -> Widget n
drawWithCentre fn =
  Widget Fixed Fixed $ do
    ctx <- getContext
    let
      w' = ctx ^. availWidthL
      w = w' - (1 - (w' `mod` 2))
      h' = ctx ^. availHeightL
      h = h' - (1 - (h' `mod` 2))
    render .
      hLimit w .
      vLimit h $
      fn w h

canSee :: OutputState -> Coord -> Bool
canSee (OutputState (V2 px py) m) (V2 x y) =
  True
  -- -- This is working on the midpoints
  -- -- Although we should be checking to see if any corners are visible
  -- -- from any of the corners of this tile
  -- -- There are also opportunities to cache visibility information here
  -- let
  --   dx = fromIntegral $ x - px
  --   dy = fromIntegral $ y - py
  --   g = dy / dx
  --   checkX cx =
  --     let
  --       cy = fromIntegral py + fromIntegral cx * g
  --       cy1 = floor cy
  --       cy2 = ceiling cy
  --     in
  --       checkC (V2 cx cy1) || checkC (V2 cx cy2)
  --   checkC c = maybe True checkT . Map.lookup c $ m
  --   checkT Wall = False
  --   checkT Floor = True
  -- in
  --   if px < x
  --   then all checkX [px+1..x-1]
  --   else all checkX [px-1..x+1]

draw :: OutputState -> [Widget n]
draw os@(OutputState (V2 px py) m) =
  let
    w =
      drawWithCentre $ \w h ->
        let
          mw = w `div` 2
          mh = h `div` 2
          rows = (\y -> hBox (cols y)) <$> [negate mh .. mh]
          cols y = (\x -> draw x y) <$> [negate mw .. mw]
          draw x y =
            if (x == 0) && (y == 0)
            then drawPlayer
            else
              let
                target = V2 (px + x) (py + y)
              in
                if canSee os target
                then drawTile (Map.lookup (V2 (px + x) (py + y)) m)
                else drawNothing
        in
          withBorderStyle BS.unicodeBold .
          B.borderWithLabel (str "Map") .
          vBox $ rows
  in
    [C.center w]

drawNothing :: Widget n
drawNothing = withAttr emptyAttr $ str " "

drawPlayer :: Widget n
drawPlayer = withAttr playerAttr $ str "@"

drawTile :: Maybe Terrain -> Widget n
drawTile Nothing = withAttr emptyAttr $ str " "
drawTile (Just Floor) = withAttr floorAttr $ str "."
drawTile (Just Wall) = withAttr wallAttr $ str " "

emptyAttr, playerAttr, floorAttr, wallAttr :: AttrName
emptyAttr = "emptyAttr"
playerAttr = "playerAttr"
floorAttr = "floorAttr"
wallAttr = "wallAttr"

attributes :: OutputState -> AttrMap
attributes _ = attrMap V.defAttr [
    (playerAttr, (V.white `on` V.black) `V.withStyle` V.bold)
  , (floorAttr, V.white `on` V.black)
  , (wallAttr, V.white `on` V.white)
  -- , (wallAttr, V.rgbColor 102 51 0 `on` V.rgbColor 102 51 0)
  ]

renderState :: OutputState
            -> ReflexBrickAppState Name
renderState os =
  ReflexBrickAppState (draw os) (const Nothing) (attributes os)

selectQuit :: Reflex t
           => EventSelector t (RBEvent e Name)
           -> Event t ()
selectQuit es =
  void . leftmost $ [
      select es (RBKey V.KEsc)
    , select es (RBKey (V.KChar 'q'))
    ]

data KeyDirection =
  KDUp | KDDown | KDLeft | KDRight
  deriving (Eq, Ord, Show, Read)

selectDirection :: Reflex t
                => EventSelector t (RBEvent e Name)
                -> Event t KeyDirection
selectDirection es =
  leftmost [
    KDUp <$ select es (RBKey V.KUp)
  , KDDown <$ select es (RBKey V.KDown)
  , KDLeft <$ select es (RBKey V.KLeft)
  , KDRight <$ select es (RBKey V.KRight)
  ]

changePos :: Map Coord Terrain
          -> KeyDirection
          -> Coord
          -> Coord
changePos m d p =
  let
    p' = case d of
      KDUp -> p & _y %~ pred
      KDDown -> p & _y %~ succ
      KDLeft -> p & _x %~ pred
      KDRight -> p & _x %~ succ
    canWalk Floor = True
    canWalk _ = False
  in
    bool p p' .
    maybe False canWalk .
    Map.lookup p' $
    m

roguelikeNetwork :: (Reflex t, MonadFix m, MonadHold t m)
                 => OutputState
                 -> EventSelector t (RBEvent Name ())
                 -> m (ReflexBrickApp t Name)
roguelikeNetwork (OutputState p m) es = do
  let
    eQuit = selectQuit es
    eDirection = selectDirection es

  dPos <- foldDyn (changePos m) p eDirection

  let
    dState = (\p -> OutputState p m)<$> dPos

  pure $ ReflexBrickApp (renderState <$> updated dState) never eQuit

drawRoom :: Coord -> Coord -> [Coord] -> Map Coord Terrain
drawRoom (V2 x1 y1)  (V2 x2 y2) doors =
  let
    roomWalls = Map.fromList $ do
      x <- [x1..x2]
      y <- [y1..y2]
      pure $ (V2 x y, Wall)
    roomFloors = Map.fromList $ do
      x <- [x1+1..x2-1]
      y <- [y1+1..y2-1]
      pure $ (V2 x y, Floor)
    doorFloors = Map.fromList $ (\c -> (c, Floor)) <$> doors
  in
    mconcat [doorFloors, roomFloors, roomWalls]

mkInitialState :: MonadIO m => m OutputState
mkInitialState =
  let
    m1 = drawRoom (V2 5 5) (V2 9 9) [V2 9 8]
    m2 = drawRoom (V2 16 5) (V2 20 9) [V2 16 8]
    m3 = drawRoom (V2 9 7) (V2 16 9) [V2 9 8, V2 16 8]
    m = mconcat [m3, m2, m1]
  in
    pure $ OutputState (V2 7 7) m

roguelike :: IO ()
roguelike = do
  initialState <- liftIO mkInitialState
  runReflexBrickApp (pure ()) (Just $ threadDelay 100000) (renderState initialState) $
    roguelikeNetwork initialState
