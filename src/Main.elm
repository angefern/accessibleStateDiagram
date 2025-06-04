port module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp, onResize, onAnimationFrame)
import Browser.Navigation exposing (..)
import Url exposing (..)

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
import String exposing (startsWith)


port sendSpeech : String -> Cmd msg


myShapes : Model -> List (Shape Msg)
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
         | BW2MP
         | MP2DW
         | MP2FW
         | FW2MP
         | FW2BW
         | BW2FW
         | MP2BW
         | LP2BW
         | BW2LP
         | NoOp

type State = TrainStation
           | ButtercupWay
           | DaffodilWay
           | MountainPass
           | FireweedWay
           | BullrushWay
           | LillyPond


stateToStr : State -> String
stateToStr state =
    case state of
        TrainStation  -> "TrainStation"
        ButtercupWay  -> "ButtercupWay"
        DaffodilWay   -> "DaffodilWay"
        MountainPass  -> "MountainPass"
        FireweedWay   -> "FireweedWay"
        BullrushWay   -> "BullrushWay"
        LillyPond     -> "LillyPond"

stateToAbbr : State -> String
stateToAbbr state = 
    case state of 
        TrainStation -> "TS"
        ButtercupWay -> "BCW"
        DaffodilWay -> "DW"
        MountainPass -> "MP"
        FireweedWay -> "FW"
        BullrushWay -> "BW"
        LillyPond -> "LP"

abbrToStateStr : String -> String
abbrToStateStr abbr = 
    case abbr of 
        "TS"  -> "TrainStation"
        "BCW" -> "ButtercupWay"
        "DW"  -> "DaffodilWay"
        "MP"  -> "MountainPass"
        "FW"  -> "FireweedWay"
        "BW"  -> "BullrushWay"
        "LP"  -> "LillyPond"
        _ -> ""


nextStates : State -> List String
nextStates state = 
    let
        abbr = stateToAbbr state
        msgs = [ "TS2BCW", "BCW2TS", "TS2DW"
                , "DW2TS", "DW2BCW", "BW2DW"
                , "BW2MP", "MP2DW", "MP2FW"
                , "FW2MP", "FW2BW", "BW2FW"
                , "MP2BW", "LP2BW", "BW2LP"
                ]
    in List.filter (startsWith abbr) msgs
        |> List.map (String.replace (abbr ++ "2") "")
        |> List.map abbrToStateStr

formatListWithOr : List String -> String
formatListWithOr list =
    case List.reverse list of
        [] ->
            ""
        last :: [] ->
            last
        last :: rest ->
            String.join ", " (List.reverse rest) ++ ", or " ++ last


stateToSpeechStr : State -> String
stateToSpeechStr state =
    let
        current = stateToStr state
        nextStr = formatListWithOr (nextStates state)
    in
        "You are at " ++ current ++ ". You may proceed to " ++ nextStr ++ "."

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        TS2BCW ->
            case model.state of
                TrainStation ->
                    ( { model | state = ButtercupWay }, sendSpeech (stateToSpeechStr ButtercupWay) )

                _ ->
                    ( model, Cmd.none )

        BCW2TS ->
            case model.state of
                ButtercupWay ->
                    ( { model | state = TrainStation }, sendSpeech (stateToSpeechStr TrainStation) )

                _ ->
                    ( model, Cmd.none )

        TS2DW ->
            case model.state of
                TrainStation ->
                    ( { model | state = DaffodilWay }, sendSpeech (stateToSpeechStr DaffodilWay) )

                _ ->
                    ( model, Cmd.none )

        DW2TS ->
            case model.state of
                DaffodilWay ->
                    ( { model | state = TrainStation }, sendSpeech (stateToSpeechStr TrainStation) )

                _ ->
                    ( model, Cmd.none )

        DW2BCW ->
            case model.state of
                DaffodilWay ->
                    ( { model | state = ButtercupWay }, sendSpeech (stateToSpeechStr ButtercupWay) )

                _ ->
                    ( model, Cmd.none )

        BW2DW ->
            case model.state of
                ButtercupWay ->
                    ( { model | state = DaffodilWay }, sendSpeech (stateToSpeechStr DaffodilWay) )

                _ ->
                    ( model, Cmd.none )

        BW2MP ->
            case model.state of
                ButtercupWay ->
                    ( { model | state = MountainPass }, sendSpeech (stateToSpeechStr MountainPass) )

                _ ->
                    ( model, Cmd.none )

        MP2DW ->
            case model.state of
                MountainPass ->
                    ( { model | state = DaffodilWay }, sendSpeech (stateToSpeechStr DaffodilWay) )

                _ ->
                    ( model, Cmd.none )

        MP2FW ->
            case model.state of
                MountainPass ->
                    ( { model | state = FireweedWay }, sendSpeech (stateToSpeechStr FireweedWay) )

                _ ->
                    ( model, Cmd.none )

        FW2MP ->
            case model.state of
                FireweedWay ->
                    ( { model | state = MountainPass }, sendSpeech (stateToSpeechStr MountainPass) )

                _ ->
                    ( model, Cmd.none )

        FW2BW ->
            case model.state of
                FireweedWay ->
                    ( { model | state = BullrushWay }, sendSpeech (stateToSpeechStr BullrushWay) )

                _ ->
                    ( model, Cmd.none )

        BW2FW ->
            case model.state of
                BullrushWay ->
                    ( { model | state = FireweedWay }, sendSpeech (stateToSpeechStr FireweedWay) )

                _ ->
                    ( model, Cmd.none )

        MP2BW ->
            case model.state of
                MountainPass ->
                    ( { model | state = BullrushWay }, sendSpeech (stateToSpeechStr BullrushWay) )

                _ ->
                    ( model, Cmd.none )

        LP2BW ->
            case model.state of
                LillyPond ->
                    ( { model | state = BullrushWay }, sendSpeech (stateToSpeechStr BullrushWay) )

                _ ->
                    ( model, Cmd.none )

        BW2LP ->
            case model.state of
                BullrushWay ->
                    ( { model | state = LillyPond }, sendSpeech (stateToSpeechStr LillyPond) )

                _ ->
                    ( model, Cmd.none )
                    
        NoOp ->
            ( model, Cmd.none )


type alias Model =
    { time : Float
    , state : State
    }

type alias Point = (Float, Float)

init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg ) --?
init _ _ _ = ({ time = 0, state = TrainStation }, sendSpeech "You are at TrainStation. You may proceed to either ButtercupWay or DaffodilWay.")


view : Model -> { title : String, body : Collage Msg }
view model = 
    { title = "My App", 
      body = collage 500 500 (myShapes model) 
    }


main : AppWithTick () Model Msg
main = 
    appWithTick Tick
        { 
            init = init,
            update = update, 
            view = view, 
            subscriptions = subscriptions, 
            onUrlRequest = \_ -> NoOp, 
            onUrlChange = \_ -> NoOp
        }