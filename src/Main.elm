module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp, onResize, onAnimationFrame)
import Browser.Navigation exposing (Key)

import List exposing (range)
import String
import Char
import Array
import Json.Decode as D
import Task
import Time exposing (..)
import Html
import Svg
import Svg.Attributes as SA


myShapes model =
    case model.state of
        TrainStation  ->
            [ text "TrainStation"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "TS2BCW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25, -25)
                     |> notifyTap TS2BCW
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "TS2DW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (25, -25)
                     |> notifyTap TS2DW
            ]
        ButtercupWay  ->
            [ text "ButtercupWay"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "BCW2TS"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-50, -25)
                     |> notifyTap BCW2TS
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "BW2DW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap BW2DW
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "BWMP"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (50, -25)
                     |> notifyTap BWMP
            ]
        DaffodilWay  ->
            [ text "DaffodilWay"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "DW2TS"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25, -25)
                     |> notifyTap DW2TS
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "DW2BCW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (25, -25)
                     |> notifyTap DW2BCW
            ]
        MountainPass  ->
            [ text "MountainPass"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "MP2DW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-50, -25)
                     |> notifyTap MP2DW
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "MP2FW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap MP2FW
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "MP2BW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (50, -25)
                     |> notifyTap MP2BW
            ]
        FireweedWay  ->
            [ text "FireweedWay"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "FW2MP"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25, -25)
                     |> notifyTap FW2MP
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "FW2BW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (25, -25)
                     |> notifyTap FW2BW
            ]
        BullrushWay  ->
            [ text "BullrushWay"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "BW2FW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (-25, -25)
                     |> notifyTap BW2FW
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "BW2LP"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (25, -25)
                     |> notifyTap BW2LP
            ]
        LillyPond  ->
            [ text "LillyPond"
                  |> centered
                  |> filled black
            , group
                  [
                       roundedRect 40 20 5
                            |> filled green
                  ,    text "LP2BW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap LP2BW
            ]

type Msg = Tick Float GetKeyState
         | TS2BCW
         | BCW2TS
         | TS2DW
         | DW2TS
         | DW2BCW
         | BW2DW
         | BWMP
         | MP2DW
         | MP2FW
         | FW2MP
         | FW2BW
         | BW2FW
         | MP2BW
         | LP2BW
         | BW2LP

type State = TrainStation
           | ButtercupWay
           | DaffodilWay
           | MountainPass
           | FireweedWay
           | BullrushWay
           | LillyPond

update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }
        TS2BCW  ->
            case model.state of
                TrainStation  ->
                    { model | state = ButtercupWay  }
                otherwise ->
                    model
        BCW2TS  ->
            case model.state of
                ButtercupWay  ->
                    { model | state = TrainStation  }

                otherwise ->
                    model
        TS2DW  ->
            case model.state of
                TrainStation  ->
                    { model | state = DaffodilWay  }

                otherwise ->
                    model
        DW2TS  ->
            case model.state of
                DaffodilWay  ->
                    { model | state = TrainStation  }

                otherwise ->
                    model
        DW2BCW  ->
            case model.state of
                DaffodilWay  ->
                    { model | state = ButtercupWay  }

                otherwise ->
                    model
        BW2DW  ->
            case model.state of
                ButtercupWay  ->
                    { model | state = DaffodilWay  }

                otherwise ->
                    model
        BWMP  ->
            case model.state of
                ButtercupWay  ->
                    { model | state = MountainPass  }

                otherwise ->
                    model
        MP2DW  ->
            case model.state of
                MountainPass  ->
                    { model | state = DaffodilWay  }

                otherwise ->
                    model
        MP2FW  ->
            case model.state of
                MountainPass  ->
                    { model | state = FireweedWay  }

                otherwise ->
                    model
        FW2MP  ->
            case model.state of
                FireweedWay  ->
                    { model | state = MountainPass  }

                otherwise ->
                    model
        FW2BW  ->
            case model.state of
                FireweedWay  ->
                    { model | state = BullrushWay  }

                otherwise ->
                    model
        BW2FW  ->
            case model.state of
                BullrushWay  ->
                    { model | state = FireweedWay  }

                otherwise ->
                    model
        MP2BW  ->
            case model.state of
                MountainPass  ->
                    { model | state = BullrushWay  }

                otherwise ->
                    model
        LP2BW  ->
            case model.state of
                LillyPond  ->
                    { model | state = BullrushWay  }

                otherwise ->
                    model
        BW2LP  ->
            case model.state of
                BullrushWay  ->
                    { model | state = LillyPond  }

                otherwise ->
                    model

type alias Model =
    { time : Float
    , state : State
    }

type alias Point = (Float, Float)

init : Model
init = { time = 0
       , state = TrainStation
       }


view model = 
    collage 500 500 (myShapes model)


main = 
    gameApp Tick
        { 
            model = init,
            view = view,  
            update = update, 
            title = "My App"
        }