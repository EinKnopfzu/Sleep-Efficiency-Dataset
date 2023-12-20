module Scatterplot exposing (..)

import Axis
import Browser
import Color exposing (Color(..), rgb)

import Debug
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (onInput)
import Http
import List exposing (filter, sum)
import Maybe.Extra exposing (isJust)
import Scale exposing (ContinuousScale)
import Stat
import Statistics exposing (extent, quantile)
import TypedSvg exposing (circle, g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, color, fill, fontFamily, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


combineLists : List Float -> List Float ->List String -> List ScatterplottPoint
combineLists x y z =
    List.map3 (\zValue xValue yValue -> { pointName = zValue, x = xValue, y = yValue }) z x y

w : Float
w =
    900

h : Float
h =
    450

padding : Float
padding =
    100
    
radius : Float
radius =
    5.0

tickCount : Int
tickCount =
    10

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

--Der Typ soll die relative Position eines Datenpunktes darstellen. 
type alias ScatterplottXYPoint =
    { xValue : Float
     ,yValue : Float}

-- XyData soll den Datensatz Beschreiben, der im Scatterplott angezeigt wird
type alias ScatterplottXYData =
    { xDescription : String
    , yDescription : String
    , data : List ScatterplottPoint
    }

--Repräsentiert den individuellen Punkt mit Name. 
type alias ScatterplottPoint =
    { pointName : String, x : Float, y : Float }

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    values
    |> Statistics.extent
    |> Maybe.withDefault defaultExtent


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

scatterplot : ScatterplottXYData -> Svg msg
scatterplot model =
    let
        {- hier können Sie die Beschriftung des Testpunkts berechnen -}
        kreisbeschriftung : String
        kreisbeschriftung =
            ""

        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues = 
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 0, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        , g
            [ transform [ Translate padding (h - padding) ]
            , TypedSvg.Attributes.class [ "x-axis" ]
            ]
            [ xAxis xValues
            , text_
                [ x 350
                , y 40
                , fontSize (TypedSvg.Types.px 20)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g
            [ transform [ Translate padding padding ]
            , TypedSvg.Attributes.class [ "y-axis" ]
            ]
            [ yAxis yValues
            , text_
                [ x 0
                , y -15
                , fontSize (TypedSvg.Types.px 20)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        , line
            [ x 0
            , y 0
            , x 100
            , y 100
            , stroke (Paint Color.blue)
            ]
            []
        ]

point : ContinuousScale Float -> ContinuousScale Float -> ScatterplottPoint -> Svg msg
point scaleX scaleY xyPoint =
    let
        ( xa, ya ) =
            ( Scale.convert scaleX xyPoint.x, Scale.convert scaleY xyPoint.y )
    in
    g [ TypedSvg.Attributes.class [ "point" ], fontSize <| Px 10.0, fontFamily [ "sans-serif" ] ]
        [ circle [ cx xa, cy ya, r radius ]
            []
        , text_
            [ x (xa + radius)
            , y (ya - radius)
            , textAnchor TypedSvg.Types.AnchorStart
            ]
            [ TypedSvg.Core.text xyPoint.pointName ]
        ]


