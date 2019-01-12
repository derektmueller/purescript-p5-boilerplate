module Main where

import Prelude 
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (toNumber)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)
import Data.Int (floor)
import Data.Array ((..))
import Data.Traversable
import Effect.Random

import P5 
import P5.Types (P5)

type AppState = {
  p5 :: P5
}

initialState :: Maybe AppState
initialState = Nothing

type Point = { x :: Number, y :: Number }

type Quad = {
  p1 :: Point,
  p2 :: Point,
  p3 :: Point,
  p4 :: Point
}

type Palette = {
  a :: String,
  b :: String,
  c :: String,
  d :: String,
  e :: String
}

drawQuad :: P5 -> Quad -> Effect Unit
drawQuad p5 quad' = do
  quad p5 
    quad'.p1.x quad'.p1.y
    quad'.p2.x quad'.p2.y
    quad'.p3.x quad'.p3.y
    quad'.p4.x quad'.p4.y

quadAddNoise :: P5 -> Quad -> Effect Quad
quadAddNoise p quad = do
  let factor = 5.0
  pure $ quad { 
    p1 = { 
      x: quad.p1.x + 
        factor * (noise p quad.p1.x (Just quad.p1.y) Nothing),
      y: quad.p1.y + 
        factor * (noise p quad.p1.x (Just quad.p1.y) Nothing)
    },
    p2 = { 
      x: quad.p2.x + 
        factor * (noise p quad.p2.x (Just quad.p2.y) Nothing),
      y: quad.p2.y + 
        factor * (noise p quad.p2.x (Just quad.p2.y) Nothing)
    },
    p3 = { 
      x: quad.p3.x + 
        factor * (noise p quad.p3.x (Just quad.p3.y) Nothing),
      y: quad.p3.y + 
        factor * (noise p quad.p3.x (Just quad.p3.y) Nothing)
    },
    p4 = { 
      x: quad.p4.x + 
        factor * (noise p quad.p3.x (Just quad.p4.y) Nothing),
      y: quad.p4.y + 
        factor * (noise p quad.p3.x (Just quad.p4.y) Nothing)
    }
  }

buildGrid :: Number -> Number -> Number -> Array Quad
buildGrid w h quadW = do
  let rowWidth = floor $ w / quadW
      colHeight = floor $ h / quadW
  x <- (0..rowWidth)
  y <- (0..colHeight)
  let x' = toNumber x * quadW
      y' = toNumber y * quadW
      padding = 5.0
  pure {
    p1: {x: x' + padding, y: y' + padding},
    p2: {x: x' + (quadW - 2.0 * padding), y: y' + padding},
    p3: {x: x' + (quadW - 2.0 * padding), 
         y: y' + (quadW - 2.0 * padding)},
    p4: {x: x' + padding, y: y' + (quadW - 2.0 * padding)}
  }

randomColor :: Palette -> Effect String
randomColor palette = do
  i <- randomInt 1 5
  pure $ case i of
    1 -> palette.a
    2 -> palette.b
    3 -> palette.c
    4 -> palette.d
    _ -> palette.e

main :: Maybe AppState -> Effect (Maybe AppState)
main mAppState = do
  win <- window
  w <- toNumber <$> innerWidth win
  h <- toNumber <$> innerHeight win
  p <- maybe getP5 (\x -> pure x.p5) mAppState

  let palette = 
        { a: "#4d0c40"
        , b: "#a11a23"
        , c: "#b29179"
        , d: "#c0a476"
        , e: "#9d7f38"
        }
  let grid = buildGrid w h 38.0

  setup p do
    _ <- createCanvas p w h
    noLoop p
    pure unit

  draw p do
    background3 p palette.e Nothing
    strokeWeight p 4.0
    traverse_ (\quad -> do
      color <- randomColor palette
      color' <- randomColor palette
      fill p color
      stroke p color'
      quad' <- quadAddNoise p quad
      drawQuad p quad') grid
    pure unit

  case mAppState of
    (Just _) ->
      redraw p Nothing
    _ -> pure unit

  pure $ Just { p5: p }
