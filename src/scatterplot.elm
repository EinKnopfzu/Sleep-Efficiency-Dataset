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

--Der Typ soll die relative Position eines Datenpunktes darstellen. 
type alias XyPoint =
    { xValue : Float
     ,yValue : Float}

-- XyData soll den Datensatz Beschreiben, der im Scatterplott angezeigt wird
type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }

--Repräsentiert den individuellen Punkt mit Name. 
type alias Point =
    { pointName : String, x : Float, y : Float }
