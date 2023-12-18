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


decoder : Decoder Unverarbeitete_Daten
decoder =
    Decoda.into Unverarbeitete_Daten
        |> Decoda.pipeline (Decoda.field "ID" (Decoda.blank Decoda.string))
        |> Decoda.pipeline (Decoda.field "Age" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Gender" (Decoda.blank Decoda.string))
        |> Decoda.pipeline (Decoda.field "Bedtime" (Decoda.blank Decoda.string))
        |> Decoda.pipeline (Decoda.field "Wakeup time" (Decoda.blank Decoda.string))
        |> Decoda.pipeline (Decoda.field "Sleep duration" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Sleep efficiency" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "REM sleep percentage" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Deep sleep percentage" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Light sleep percentage" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Awakenings" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Caffeine consumption" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Alcohol consumption" (Decoda.blank Decoda.float))
        |> Decoda.pipeline (Decoda.field "Smoking status" (Decoda.blank Decoda.string))
        |> Decoda.pipeline (Decoda.field "Exercise frequency" (Decoda.blank Decoda.float))