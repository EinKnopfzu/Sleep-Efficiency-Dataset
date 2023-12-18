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


car2Point : Car -> Maybe MyCar
car2Point c =
    Maybe.map5
        (\myRetail myCityMpg myVehicleName mydealerCost mycarLen ->
            MyCar
                (toFloat myRetail)
                (toFloat myCityMpg)
                (myVehicleName
                    ++ "("
                    ++ String.fromInt myCityMpg
                    ++ ","
                    ++ String.fromInt myRetail
                    ++ ")"
                )
                (toFloat mydealerCost)
                (toFloat mycarLen)
                0.0
                0.0
                0.0
        )
        c.retailPrice
        c.cityMPG
        (Just c.vehicleName)
        c.dealerCost
        c.carLen
        |> Maybe.andThen
            (\cX ->
                Maybe.map3
                    (\myXA myXB myXC ->
                        { cX | myweight = toFloat myXA, mycarWidth = toFloat myXB, myengineSize = myXC }
                    )
                    c.weight
                    c.carWidth
                    c.engineSize
            )
