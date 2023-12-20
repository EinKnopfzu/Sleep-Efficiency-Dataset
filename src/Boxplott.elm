module Boxplott exposing (..)

import Axis
import Color exposing (Color(..), rgb)
import Html exposing (Html)
import List exposing (sum)
import List.Extra exposing (transpose)
import Scale exposing (ContinuousScale)
import Shape
import Stat
import Statistics exposing (extent, quantile)
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, color, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Path exposing (Path)


