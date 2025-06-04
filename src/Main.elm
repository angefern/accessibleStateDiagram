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
    let
        currentFocus =
            case model.nextStatesZip of
                SZip _ f _ -> f

        highlightIfFocused : State -> State -> Shape Msg
        highlightIfFocused focusedState thisState =
            roundedRect 40 20 5
                |> filled (if focusedState == thisState then pink else green)
    in
    case model.state of
        TrainStation  ->
            [ text "TrainStation"
                  |> centered
                  |> filled black
            , group
                  [
                       highlightIfFocused currentFocus ButtercupWay
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
                       highlightIfFocused currentFocus DaffodilWay                      
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
                       highlightIfFocused currentFocus TrainStation                      
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
                       highlightIfFocused currentFocus DaffodilWay
                  ,    text "BCW2DW"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (0, -25)
                     |> notifyTap BCW2DW
            , group
                  [
                       highlightIfFocused currentFocus MountainPass
                  ,    text "BCW2MP"
                            |> centered
                            |> size 8
                            |> filled black
                            |> move(0, -3)
                  ]
                     |> move (50, -25)
                     |> notifyTap BCW2MP
            ]
        DaffodilWay  ->
            [ text "DaffodilWay"
                  |> centered
                  |> filled black
            , group
                  [
                       highlightIfFocused currentFocus TrainStation
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
                       highlightIfFocused currentFocus ButtercupWay
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
                       highlightIfFocused currentFocus DaffodilWay
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
                       highlightIfFocused currentFocus FireweedWay
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
                       highlightIfFocused currentFocus BullrushWay
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
                       highlightIfFocused currentFocus MountainPass
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
                       highlightIfFocused currentFocus BullrushWay
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
                       highlightIfFocused currentFocus FireweedWay
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
                       highlightIfFocused currentFocus LillyPond
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
                       highlightIfFocused currentFocus BullrushWay
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
         | BCW2DW
         | BCW2MP
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

type StateZipper = SZip (List State) State (List State) -- before, focused, after

sPrev : StateZipper -> StateZipper
sPrev (SZip before focused after) = case before of
                                      [] -> SZip before focused after
                                      a::rest -> SZip rest a (focused::after)

sNext : StateZipper -> StateZipper
sNext  (SZip before focused after) = case after of
                                         [] -> SZip before focused after
                                         a::rest -> SZip (focused::before) a rest


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

strToState : String -> Maybe State
strToState str =
    case str of
        "TrainStation" -> Just TrainStation
        "ButtercupWay" -> Just ButtercupWay
        "DaffodilWay" -> Just DaffodilWay
        "MountainPass" -> Just MountainPass
        "FireweedWay" -> Just FireweedWay
        "BullrushWay" -> Just BullrushWay
        "LillyPond" -> Just LillyPond
        _ -> Nothing



nextStatesStr : State -> List String
nextStatesStr state = 
    let
        abbr = stateToAbbr state
        msgs = [ "TS2BCW", "BCW2TS", "TS2DW"
                , "DW2TS", "DW2BCW", "BCW2DW"
                , "BCW2MP", "MP2DW", "MP2FW"
                , "FW2MP", "FW2BW", "BW2FW"
                , "MP2BW", "LP2BW", "BW2LP"
                ]
    in List.filter (startsWith abbr) msgs
        |> List.map (String.replace (abbr ++ "2") "")
        |> List.map abbrToStateStr

nextStates : List String -> List State
nextStates states = List.map (\s -> Maybe.withDefault TrainStation (strToState s)) states 

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
        nextStr = formatListWithOr (nextStatesStr state)
    in
        "You are at " ++ current ++ ". You may proceed to " ++ nextStr ++ "."

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick t (keys, _, _) ->
            if keys RightArrow == JustDown then 
                ({ model | time = t
                    , nextStatesZip = sNext model.nextStatesZip
                    }, Cmd.none)
            else if keys LeftArrow == JustDown then 
                ({ model | time = t
                    , nextStatesZip = sPrev model.nextStatesZip
                    }, Cmd.none)
            else if keys Enter == JustDown then 
                case model.nextStatesZip of 
                    SZip _ focused _ -> 
                        case nextStates (nextStatesStr focused) of
                            a :: rest -> 
                                ({ model | time = t
                                    , state = focused
                                    , nextStatesZip = SZip [] a rest
                                    }, sendSpeech (stateToSpeechStr focused))
                            [] -> ( { model | time = t }, Cmd.none )

            else 
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

        BCW2DW ->
            case model.state of
                ButtercupWay ->
                    ( { model | state = DaffodilWay }, sendSpeech (stateToSpeechStr DaffodilWay) )

                _ ->
                    ( model, Cmd.none )

        BCW2MP ->
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
    , nextStatesZip : StateZipper
    }

type alias Point = (Float, Float)

init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg ) --?
init _ _ _ = ({ time = 0, state = TrainStation, nextStatesZip = (SZip [] ButtercupWay [DaffodilWay]) }
                , sendSpeech "You are at TrainStation. You may proceed to either ButtercupWay or DaffodilWay." )


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