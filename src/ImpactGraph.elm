module ImpactGraph exposing (..)

import Scatterplot exposing (w, h, padding, radius, tickCount)

import JXstat exposing (..)
import TypedSvg exposing (circle, g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, color, fill, fontFamily, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import TypedSvg.Attributes.InPx exposing (x1)
import TypedSvg.Attributes.InPx exposing (y1)
import TypedSvg.Attributes.InPx exposing (x2)
import TypedSvg.Attributes.InPx exposing (y2)
import TypedSvg.Attributes exposing (x1)


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

type alias ImpaxtGraphData =
    { xdescriptor : String
    , ydescriptor : String
    , zdescriptor : String
    , ddescriptor : String
    , kdescriptor : String
    , data : List (ImpactGrapPoint)
    }

type alias ImpactGrapPoint =
    { xValue : Float
     ,yValue : Float
     ,zValue : Float
     ,dValue : Float
     , kValue : Float}


-- Ziel ist es eine Grafik zu erschaffen di eim zentrum unseren X Datensatz hat (Sagen wir als Quader . ) und den in de rMitte Zeichnet. 
-- dann geben wi reine distanz an. das ist die maximale Distanz bei den X Werten zum Quader. 
-- Die Quader werden dann um den Quader herum gezeichnet 
graph : ImpaxtGraphData -> Svg msg
graph model =
    let

        XWert : List Float
        XWert = List.map .xValue model.data

        yWert : List Float
        yWert = List.map .yValue model.data

        zWert : List Float  
        zWert = List.map .zValue model.data

        dWert : List Float
        dWert = List.map .dValue model.data

        kWert : List Float
        kWert = List.map .kValue model.data


        combineLists : List Float -> List Float -> List (Float, Float)
        combineLists list1 list2 =
            List.map2 Tuple.pair list1 list2

        dataY : List ( Float, Float )
        dataY =  combineLists xWert yWert
        

        rValue : Maybe Float
        rValue = r data

    in
    svg [ viewBox 0 200 (w) ( h - 300), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0, 0.4); fill: rgba(255, 255, 255, 0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
   
        ]


