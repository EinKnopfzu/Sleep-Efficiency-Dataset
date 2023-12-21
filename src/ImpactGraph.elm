module ImpactGraph exposing (..)

import Scatterplot exposing (w, h, padding, radius, tickCount)

import JXstat exposing (..)
import TypedSvg exposing (svg)


--etwas hindernd, dass ELM keine Rückewärts kompatibilität erzwingt und man so nicht die neusten Versionen nutzen kann, wenn diese Abhängig sind

{-Grundsätzlicher Plan:

Ziel ist es die korrelation des gewählten Attributes auf die Variable x zu berechnen. 
Hier zu wollen wir ausgewählte Einflussfaktoren nehmen und auf den ausgewählten Datensatz x berechnen sowie auf die gewählten Werte y und z.

Die Korrelation wird repräsentiert durch r 
0 bis 1 = Positive korrelation
-1 bis o = negative korrelation 

Nähe der Werte zu 0 = keine Korrelation
Im Zentrum ist das gewählte Attribut x oder y oder z
un drum herum sind die Einflussfaktoren die nähe gibt die Korrelation an
Und die Farbe die stärke der Korrelation 

-}
--Gibt die Summe der Liste zurück
summe : List Float -> Float
summe list = 
    List.sum list

--Correlation von X auf Y 
r : List ( Float, Float ) -> Maybe Float
r data =   
    correlation data

{-

graph : svg msg
graph =
    let

    in
    svg [ viewBox 0 200 (w) ( h - 300), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        , g
            [ transform [ Translate padding (h - padding) ]
            , class [ "x-axis" ]
            ]
            [ xAxis xValues
            , text_
                [ x (400)                
                , y ( 40)
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ Html.text model.xDescription]
            ]
        , g
            [ transform [ Translate padding padding ]
            , class [ "y-axis" ]
            ]
            [ yAxis yValues
            , text_
                [ x 0
                , y -15
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ Html.text model.xDescription ]
            ]
        
        , g [ transform [ Translate padding padding ] ]
            [ line
              [ TypedSvg.Attributes.InPx.x1 (xS)
                , TypedSvg.Attributes.InPx.y1 (yS)
                , TypedSvg.Attributes.InPx.x2 (xE)
                , TypedSvg.Attributes.InPx.y2 (yE)
                , TypedSvg.Attributes.InPx.strokeWidth 2  -- Adjusted stroke width
                , stroke <| Paint <| Color.rgba 255 0 0 1
                ]    []
            ]   
        ]


-}