module ImpactGraph exposing (..)

import Scatterplot exposing (w, h, padding, radius, tickCount, defaultExtent)

import List exposing (..)
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
import Html exposing (..)
import Scale exposing (ContinuousScale) 

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

type alias ImpactGraphData =
    { xdescriptor : ImpactDataList
    , ydescriptor : ImpactDataList
    , zdescriptor : ImpactDataList
    , ddescriptor : ImpactDataList
    , kdescriptor : ImpactDataList
    , fdescriptor : ImpactDataList
    , ldescriptor : ImpactDataList
    
    }


type alias ImpactDataList =
    { name : String
    , data : List (Float)

    }


type alias ImpactGraphPoint =
    { xValue : Float
     ,yValue : Float
     ,zValue : Float
     ,dValue : Float
     , kValue : Float}


-- Ziel ist es eine Grafik zu erschaffen di eim zentrum unseren X Datensatz hat (Sagen wir als Quader . ) und den in de rMitte Zeichnet. 
-- dann geben wi reine distanz an. das ist die maximale Distanz bei den X Werten zum Quader. 
-- Die Quader werden dann um den Quader herum gezeichnet 
graph : ImpactGraphData -> Svg msg
graph model =
    let

-- Die Lösung die Listen so "hardgecoded" herieinzuspielen is unschön Besser wäre es, die zwei Typen zu übergeben: 
-- List (List Float) Für die Variablen die wir überprüfen und dann die Daten auszupacken. Aber da wir nur limitierte 
-- Verhalteneigenschaftenhaben: Alter, 
        xWert : String
        xWert =   model.xdescriptor.name

        yWert : String
        yWert = model.ydescriptor.name

        zWert : String  
        zWert = model.zdescriptor.name

        dWert : String
        dWert = model.ddescriptor.name

        kWert : String 
        kWert = model.kdescriptor.name

        combineLists : List Float -> List Float -> List (Float, Float)
        combineLists list1 list2 =
            List.map2 Tuple.pair list1 list2


        listYX : List(Float, Float) 
        listYX = combineLists model.ydescriptor.data model.xdescriptor.data

        rY : Maybe Float
        rY = r listYX 

        listZX : List(Float, Float )
        listZX =  combineLists model.zdescriptor.data model.xdescriptor.data

        rZ : Maybe Float
        rZ = r listZX

        listDX : List( Float, Float )
        listDX =  combineLists model.ddescriptor.data model.xdescriptor.data

        rD : Maybe Float
        rD = r listDX

        listKX : List( Float, Float )
        listKX =  combineLists model.kdescriptor.data model.xdescriptor.data

        rK : Maybe Float
        rK = r listKX


        xa = 200/2
        ya = h/2

        radius = 40

    in
    svg [ viewBox 0 200 (w) ( h - 300), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0, 0.4); fill: rgba(255, 255, 255, 0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate padding padding ] ]
            [ circle [ cx xa, cy ya, TypedSvg.Attributes.InPx.r 50 ]
            []
            ]           --(   List.map (point xScaleLocal yScaleLocal) model.data)
   
        ]

{-
kreis : ContinuousScale Float -> ContinuousScale Float -> ScatterplottPoint -> Svg msg
kreis scaleX scaleY xyPoint =
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


type alias ScatterplottPoint =
    { pointName : String, x : Float, y : Float }

type alias ScatterplottXYPoint =
    { xValue : Float
     ,yValue : Float}
     -}