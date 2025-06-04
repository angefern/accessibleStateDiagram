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
         | KeyDown String
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


stateToSpeechStr : State -> StateZipper -> String
stateToSpeechStr state zip =
    let
        current = stateToStr state
        nextStr = formatListWithOr (nextStatesStr state)
    in
        case zip of 
            SZip _ focused _ -> "You are at " ++ current ++ ". You may proceed to " ++ nextStr ++ ". " ++ (stateToStr focused) ++ " is currently selected. Use the left and right arrow keys to change selection."

subscriptions : Model -> Sub Msg
subscriptions _ =
  onKeyDown (D.map KeyDown (D.field "key" D.string))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick t _ -> if model.timeSinceLastSpoken >= 15
                    then (
                        case model.state of
                            TrainStation -> ( { model | time = t, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr TrainStation model.nextStatesZip) )
                            ButtercupWay -> ( { model | time = t, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr ButtercupWay model.nextStatesZip) )
                            DaffodilWay -> ( { model | time = t, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr DaffodilWay model.nextStatesZip) )
                            MountainPass -> ( { model | time = t, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr MountainPass model.nextStatesZip) )
                            FireweedWay -> ( { model | time = t, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr FireweedWay model.nextStatesZip) )
                            BullrushWay -> ( { model | time = t, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr BullrushWay model.nextStatesZip) )
                            LillyPond -> ( { model | time = t, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr LillyPond model.nextStatesZip) )
                    )
                    else    ( { model | time = t, timeSinceLastSpoken = model.timeSinceLastSpoken + 0.01 }, Cmd.none )


        TS2BCW ->
            case model.state of
                TrainStation ->
                    let
                        nextStatesList = nextStates (nextStatesStr ButtercupWay)
                        
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = ButtercupWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr ButtercupWay model.nextStatesZip) )
                            _ -> ( { model | state = ButtercupWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr ButtercupWay model.nextStatesZip) )
                _ ->
                    ( model, Cmd.none )

        BCW2TS ->
            case model.state of
                ButtercupWay ->
                    let
                        nextStatesList = nextStates (nextStatesStr TrainStation)
                        
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = TrainStation, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr TrainStation model.nextStatesZip) )
                            _ -> ( { model | state = TrainStation, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr TrainStation model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        TS2DW ->
            case model.state of
                TrainStation ->
                    let
                        nextStatesList = nextStates (nextStatesStr DaffodilWay)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = DaffodilWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr DaffodilWay model.nextStatesZip) )
                            _ -> ( { model | state = DaffodilWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr DaffodilWay model.nextStatesZip) )
                _ ->
                    ( model, Cmd.none )

        DW2TS ->
            case model.state of
                DaffodilWay ->
                    let
                        nextStatesList = nextStates (nextStatesStr TrainStation)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = TrainStation, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr TrainStation model.nextStatesZip) )
                            _ -> ( { model | state = TrainStation, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr TrainStation model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        DW2BCW ->
            case model.state of
                DaffodilWay ->
                    ( { model | state = ButtercupWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr ButtercupWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        BCW2DW ->
            case model.state of
                ButtercupWay ->
                    let
                        nextStatesList = nextStates (nextStatesStr DaffodilWay)
                    in
                        case nextStatesList of 
                            a::rest -> ( { model | state = DaffodilWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr DaffodilWay model.nextStatesZip) )
                            _ -> ( { model | state = DaffodilWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr DaffodilWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        BCW2MP ->
            case model.state of
                ButtercupWay ->
                    let
                        nextStatesList = nextStates (nextStatesStr MountainPass)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = MountainPass, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr MountainPass model.nextStatesZip) )
                            _ -> ( { model | state = MountainPass, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr MountainPass model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        MP2DW ->
            case model.state of
                MountainPass ->
                    let
                            nextStatesList = nextStates (nextStatesStr DaffodilWay)
                    in
                        case nextStatesList of 
                            a::rest -> ( { model | state = DaffodilWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr DaffodilWay model.nextStatesZip) )
                            _ -> ( { model | state = DaffodilWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr DaffodilWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        MP2FW ->
            case model.state of
                MountainPass ->
                    let
                            nextStatesList = nextStates (nextStatesStr FireweedWay)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = FireweedWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr FireweedWay model.nextStatesZip) )
                            _ -> ( { model | state = FireweedWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr FireweedWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        FW2MP ->
            case model.state of
                FireweedWay ->
                    let
                            nextStatesList = nextStates (nextStatesStr MountainPass)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = MountainPass, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr MountainPass model.nextStatesZip) )
                            _ -> ( { model | state = MountainPass, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr MountainPass model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        FW2BW ->
            case model.state of
                FireweedWay ->
                    let
                            nextStatesList = nextStates (nextStatesStr BullrushWay)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = BullrushWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr BullrushWay model.nextStatesZip) )
                            _ -> ( { model | state = BullrushWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr BullrushWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        BW2FW ->
            case model.state of
                BullrushWay ->
                    let
                            nextStatesList = nextStates (nextStatesStr FireweedWay)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = FireweedWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr FireweedWay model.nextStatesZip) )
                            _ -> ( { model | state = FireweedWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr FireweedWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        MP2BW ->
            case model.state of
                MountainPass ->
                    let
                            nextStatesList = nextStates (nextStatesStr BullrushWay)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = BullrushWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr BullrushWay model.nextStatesZip) )
                            _ -> ( { model | state = BullrushWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr BullrushWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        LP2BW ->
            case model.state of
                LillyPond ->
                    let
                            nextStatesList = nextStates (nextStatesStr BullrushWay)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = BullrushWay, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr BullrushWay model.nextStatesZip) )
                            _ -> ( { model | state = BullrushWay, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr BullrushWay model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )

        BW2LP ->
            case model.state of
                BullrushWay ->
                    let
                            nextStatesList = nextStates (nextStatesStr LillyPond)
                    in
                        case nextStatesList of
                            a::rest -> ( { model | state = LillyPond, timeSinceLastSpoken = 0, nextStatesZip = SZip [] a rest }, sendSpeech (stateToSpeechStr LillyPond model.nextStatesZip) )
                            _ -> ( { model | state = LillyPond, timeSinceLastSpoken = 0 }, sendSpeech (stateToSpeechStr LillyPond model.nextStatesZip) )

                _ ->
                    ( model, Cmd.none )
                    
        NoOp ->
            ( model, Cmd.none )
        KeyDown code -> case code of
                            "ArrowRight" -> let
                                                updatedZipper = sNext model.nextStatesZip
                                
                                            in
                                                ({ model | nextStatesZip = updatedZipper, timeSinceLastSpoken = 0  }, sendSpeech (focusChangeStr updatedZipper))
                            "ArrowLeft" ->  let
                                                updatedZipper = sPrev model.nextStatesZip
                                            in
                                                ({ model | nextStatesZip = updatedZipper, timeSinceLastSpoken = 0 }, sendSpeech (focusChangeStr updatedZipper))
                            "Enter" -> (case model.nextStatesZip of
                                            SZip _ focused _ -> 
                                                case nextStates (nextStatesStr focused) of
                                                    a::rest -> ({ model | state = focused
                                                                        , nextStatesZip = SZip [] a rest
                                                                        , timeSinceLastSpoken = 0
                                                                 }, sendSpeech (stateToSpeechStr focused (SZip [] a rest)))

                                                    [] -> (model, Cmd.none) 
                                        )
                            _ -> (model, Cmd.none)
        
        
        {-( model
                        {-case model.state of
                            NotTyping -> model -}
                            -- TODO handle keyboard input here
                            -- ExampleState -> { model | chars1 = typeAndDelete model.chars1 code }
                        ,Cmd.none)-}


focusChangeStr : StateZipper -> String
focusChangeStr zip = case zip of
                        SZip _ focused _ -> "You have selected " ++ (stateToStr focused) ++ ". Press enter to move to this state."

type alias Model =
    { time : Float
    , state : State
    , nextStatesZip : StateZipper
    , timeSinceLastSpoken : Float
    }

type alias Point = (Float, Float)

init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg ) --?
init _ _ _ = ({ time = 0, state = TrainStation, nextStatesZip = (SZip [] ButtercupWay [DaffodilWay]), timeSinceLastSpoken = 0 }
                , sendSpeech "You are at TrainStation. You may proceed to ButtercupWay or DaffodilWay. ButtercupWay is currently selected." )



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