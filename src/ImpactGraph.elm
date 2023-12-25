module ImpactGraph exposing (..)

import Scatterplot exposing (w, h, padding, radius, tickCount, defaultExtent)

import List exposing (..)
import JXstat exposing (..)
import TypedSvg exposing (circle, g, line, rect, style, svg, text_, tspan)
import TypedSvg.Attributes exposing (class, color, fill, fontFamily, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y, rx, x1, x2, y1, y2, dy, dx)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))

import TypedSvg.Attributes exposing (x1)
import Html exposing (text, br)
import Scale exposing (ContinuousScale) 
import Round exposing (round)

import Color exposing (rgba)
import Color exposing (Color)
import TypedSvg.Types exposing (px)

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

--radiusUmkreis : Float
--radiusUmkreis = 600


type alias ImpactGraphData =
    { xdescriptor : ImpactData
    , attribute : List ImpactData    
    }


type alias ImpactData =
    { name : String
    , data : List (Float)
    }

type alias IndexedImpactData =
    { index : Int
    , data : ImpactData
    }

runden : Float -> Float
runden nr =
     Maybe.withDefault 0.0 (String.toFloat (Round.ceiling 4 nr))

type alias ImpactGraphPoint =
    { xPosition: Float
    , yPosition: Float
    , yValue : List (Float, Float)}

r : List ( Float, Float ) -> Maybe Float
r data =   
    correlation data

combineLists : List Float -> List Float -> List (Float, Float)
combineLists list1 list2 =
            List.map2 Tuple.pair list1 list2

-- Ziel ist es eine Grafik zu erschaffen di eim zentrum unseren X Datensatz hat (Sagen wir als Quader . ) und den in de rMitte Zeichnet. 
-- dann geben wi reine distanz an. das ist die maximale Distanz bei den X Werten zum Quader. 
-- Die Quader werden dann um den Quader herum gezeichnet 
graph : ImpactGraphData -> Svg msg
graph model =
    let

-- Die Lösung die Listen so "hardgecoded" herieinzuspielen is unschön Besser wäre es, die zwei Typen zu übergeben: 
-- List (List Float) Für die Variablen die wir überprüfen und dann die Daten auszupacken. Aber da wir nur limitierte 
-- Verhalteneigenschaftenhaben: Alter, 
        anzahlPunkte : Int
        anzahlPunkte = List.length model.attribute

        indiziereImpactData : List ImpactData -> List (IndexedImpactData)
        indiziereImpactData impactDataList =
          List.indexedMap (\index impactData -> { index = index, data = impactData }) impactDataList


        indexedimpactDataList : List (IndexedImpactData)
        indexedimpactDataList = indiziereImpactData model.attribute

        attributDaten : ImpactData
        attributDaten = model.xdescriptor

        winkelEinteilung : Float
        winkelEinteilung = 2.0 * pi / toFloat anzahlPunkte

        xa = w/2 - padding * 0.5
        ya = h/2 

        headerY : Float
        headerY =  30


    in
    svg [ viewBox 0 0 (w) ( h - 300), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0, 0.4); fill: rgba(255, 255, 255, 0.3); }
            .point text { display: inline; }
            .point:hover circle { stroke: rgba(0, 0, 0, 1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
       , g [ transform [ Translate padding 0 ] ]
            [  text_
                [ x (0)                
                , y ( padding * -1 )
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorStart
                ]
                [ Html.text "Diese Graphen repräsentation soll Ihenn dabei helfen die Korrelation der Verhaltensweisen auf die Merkmale."
                 ]]
       , g [ transform [ Translate padding 0 ] ]
            [  text_
                [ x (0)                
                , y ( padding *(-1)  - 16 )
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorStart
                ]
                [ Html.text "Diese Graphen repräsentation soll Ihenn dabei helfen die Korrelation der Verhaltensweisen auf die Merkmale."
                 ]]
       , g [ transform [ Translate padding padding ] ]
            [ rect
            [ TypedSvg.Attributes.InPx.x1 (5)
            , TypedSvg.Attributes.InPx.y1 (5)
            , TypedSvg.Attributes.InPx.width (w - padding - 5 )
            , TypedSvg.Attributes.InPx.height (h - 5)
            , rx 15
            , stroke <| Paint <| Color.rgb 0 0 0 
            ,fill PaintNone
            
            ]
            [] 
            ]
        , g [transform [ Translate padding padding]]
             ( List.map (\i -> kreis xa ya model.xdescriptor i.index i.data winkelEinteilung 100) indexedimpactDataList)
--        , g [ transform [ Translate padding padding ] ]
   --          ( List.map (\i -> Pfeil))

        , g [ transform [ Translate padding padding ] ]
            [ text_
            [ x (xa )
             , y (headerY)
             , textAnchor TypedSvg.Types.AnchorMiddle
             , fontSize (TypedSvg.Types.px 30)
            ]
            [ TypedSvg.Core.text model.xdescriptor.name ] 
            ]
    
        , g [ transform [ Translate padding padding ] ]
            [ circle [ cx xa, cy ya, TypedSvg.Attributes.InPx.r (Scatterplot.radius * 3) ]
            []
            , text_
            [ x (xa + radius)
             , y (ya - radius)
             , textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [-- TypedSvg.Core.text model.xdescriptor.name ] 
            ]]

        ]


kreis : Float-> Float -> ImpactData-> Int -> ImpactData-> Float -> Float ->  Svg msg
kreis xa ya xAttribut index datenwerte winkel radiusUmkreis=
    let
        xPosition : Float
        xPosition = xa + (cos (toFloat index * winkel) * radiusUmkreis)

        yPosition : Float
        yPosition = ya + (sin (toFloat index * winkel) * radiusUmkreis)

-- Berechnet die Korrelation und rundet sie auf die 4 Nachkommastelle auf. 
        größe :   Float
        größe = runden (Maybe.withDefault 0.0 ( (r (combineLists xAttribut.data datenwerte.data))))

        rgbScale : ContinuousScale Float
        rgbScale  =
            Scale.linear ( 0, 255 ) (0, 1)

        colorTon : Float
        colorTon = 
            Scale.convert rgbScale größe

        radiusScale : ContinuousScale Float
        radiusScale  =
            Scale.linear ( 9, 1 ) (20, 70)

        scaledRadius : Float
        scaledRadius = 
            Scale.convert radiusScale größe

        farbe : Color
        farbe = if größe <= 0.0 
                then  Color.rgba 1 0 0 größe
                else Color.rgba 0 1 0 größe      

    in
    g [ ]
        [ circle [ cx xPosition
                  , cy yPosition
                  , TypedSvg.Attributes.InPx.r (scaledRadius)
   --               , TypedSvg.Attributes.InPx.r (sqrt(größe * größe) * Scatterplot.radius*3 ) 
                  , fill <| Paint <| farbe]
                   []
        , text_
            [ x (xPosition )
            , y (yPosition)
            , textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [ TypedSvg.Core.text (datenwerte.name ++ ": "++ String.fromFloat größe) ]
        , line xa ya xPosition yPosition
             ]

line : Float -> Float -> Float -> Float -> Svg msg
line x1 y1 x2 y2 =
    TypedSvg.line [
        TypedSvg.Attributes.InPx.x1 (x1)
       , TypedSvg.Attributes.InPx.y1 (y1)
       , TypedSvg.Attributes.InPx.x2 (x2)
       , TypedSvg.Attributes.InPx.y2 (y2)
       , stroke <| Paint <| Color.rgb 0 0 0
       ]
       []


