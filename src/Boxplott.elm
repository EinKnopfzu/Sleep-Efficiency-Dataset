module Boxplott exposing (..)

import Scatterplot exposing (..) 
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



w : Float
w =
    900


h : Float
h =
    550


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


combineLists_Box : List String -> List Float -> List Float -> List Float -> List MultiDimPoint
combineLists_Box names list1 list2 list3 =
    List.map4
        (\name val1 val2 val3 ->
            { pointName = name
            , value = [val1, val2, val3]
            }
        )
        names
        list1
        list2
        list3
        
boxplott :List (MultiDimPoint) -> Svg msg
boxplott model =
    let
        {- hier können Sie die Beschriftung des Testpunkts berechnen -}
        kreisbeschriftung : String
        kreisbeschriftung =
            ""

        -- Zwei Probleme 1: Skalierung der Linien,
        -- Skalieren der Punkte auf die relative x- und y-Position
        -- Umwandeln der MultiDimPoint in eine Liste von Floats


        -- Alle Punkte in Liste(Float)
        delta : List (List Float)
        delta =
            List.map .value model

        --minMax der Dimension
        datenReichweite =
            delta
                |> List.Extra.transpose
                |> List.map (\l -> ( Maybe.withDefault -1 (List.minimum l), Maybe.withDefault -1 (List.maximum l) ))

        -- Anzahl DImensionen
        dimension =
            List.length datenReichweite

        -- Skalierung für die X-Achse
        xScaling : ContinuousScale Float
        xScaling =
            Scale.linear ( toFloat 0, w * 0.8 ) ( toFloat 1, toFloat dimension )

        scalingY : List (ContinuousScale Float)
        scalingY =
            List.map (\x -> Scale.linear ( 0, h - 2*padding ) ( Tuple.first x, Tuple.second x )) datenReichweite

        --Rechnet die Werte auf die Skalierung der Dimension
    
        -- Zeichnet eine X achse
        achsenZeichner i achse =
            g [ transform [ Translate (Scale.convert xScaling (toFloat (i + 1))) 0 ] ] [ achse ]

        -- Zeichnet die Achsen auf ihrer x Position ein.
        achsenverschoben =
            List.indexedMap (\i -> \x -> achsenZeichner i x) achsen

        achsen =
            List.indexedMap
                (\index data -> Axis.left [ Axis.tickCount tickCount ] data)
                scalingY

        -- Hinzufügen der Dimension.
        dimensionsindicator : List Float
        dimensionsindicator =
            List.indexedMap (\index _ -> Scale.convert xScaling (toFloat index + 1)) datenReichweite

     
        pfadliste : List (List (Maybe ( Float, Float )))
        pfadliste =
            List.map (\x -> List.map3 (\a b c -> Just ( a, Scale.convert b c )) dimensionsindicator scalingY x) delta

    
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g []
            [rect
                [ x ( 0)
                , y ( 0 )
                , width ( w)
                , height ( h)
                , fill (Paint <| Color.rgba 0 0 0 1)
                ]
            []
            ]
        , g
            [ transform [ Translate padding padding ]
            , class [ "x-axis" ]
            ]
            achsenverschoben
        , g
            [ transform [ Translate padding padding ]
            , class [ "y-axis" ]
            ]
            [ text_
                [ x 0
                , y -15
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ text "Placeholder" ]
            ]
        , g
            [ transform [ Translate padding padding ]
            , class [ "x-axis" ]
            ]
            (List.map
                (\x ->
                    Path.element (Shape.line Shape.linearCurve x)
                        [ stroke (Paint <| Color.rgba 255 255 255 0.1)
                        , strokeWidth (TypedSvg.Types.px 2)
                        , fill PaintNone
                        ]
                )
                pfadliste
            )
        ]






type alias FilteredData = 
    { vehicleName : String
    , cityMPG : Float
    , retailPrice :Float
    , dealerCost : Float
    , carLen : Float }


type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }
    



type alias Point =
    { pointName : String, x : Float, y : Float }


      


{-}
lengthnofilter : String
lengthnofilter =
    String.fromInt (List.length cars)


avgMpg : List Car -> Float
avgMpg myCars =
    let
        cityMpgs =
            List.filterMap (\car -> car.cityMPG) myCars

        totalCityMpg =
            sum cityMpgs

        totalCars =
            List.length cityMpgs
    in
    if totalCars == 0 then
        0

    else
        toFloat totalCityMpg / toFloat totalCars

-}
