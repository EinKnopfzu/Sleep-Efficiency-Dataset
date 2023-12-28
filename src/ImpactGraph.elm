module ImpactGraph exposing (..)

import Scatterplot exposing (w, h, padding, radius)

import List exposing (..)
import JXstat exposing (..)
import TypedSvg exposing (circle, g, line, rect,  svg, text_)
import TypedSvg.Attributes exposing ( fill,  fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy,  r,  x, y, rx, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import TypedSvg.Attributes exposing (x1)

import Scale exposing (ContinuousScale) 
import Round exposing (ceiling)
import Color exposing (Color, rgba)
import Scale.Color exposing (redsInterpolator, greensInterpolator)

--etwas hindernd, dass ELM keine Rückewärts kompatibilität erzwingt und man so nicht die neusten Versionen nutzen kann, wenn diese Abhängig sind


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

runden : Float -> Float
runden nr =
     Maybe.withDefault 0.0 (String.toFloat (Round.ceiling 5 nr))
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
    svg [ viewBox 0 0 (w) ( h ), TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0, 0.4); fill: rgba(255, 255, 255, 0.3); }
            .point text { display: inline; }
            .point:hover circle { stroke: rgba(255, 255, 255, 1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
            .hoverText:hover text_ { display: inline; stroke: rgba(255, 255, 0, 1.0);  }
            .hoverText text_ { display: none; }
          """ ]
       , g [ transform [ Translate padding 0] ]
            [ rect
            [ TypedSvg.Attributes.InPx.x1 (5)
            , TypedSvg.Attributes.InPx.y1 (5)
            , TypedSvg.Attributes.InPx.width (w - padding - 5 )
            , TypedSvg.Attributes.InPx.height (h - padding - 5)
            , rx 15
            , stroke <| Paint <| Color.rgb 0 0 0 
            ,fill PaintNone
            
            ]
            [] 
            ]
        , g [transform [ Translate padding 0]]
                
             ( List.map (\i -> kreis xa ya model.xdescriptor i.index i.data winkelEinteilung 130) indexedimpactDataList)
        , g [ transform [ Translate padding 0 ] ]
            [ text_
            [ x (xa )
             , y (headerY)
             , textAnchor TypedSvg.Types.AnchorMiddle
             , fontSize (TypedSvg.Types.px 30)
            ]
            [ TypedSvg.Core.text model.xdescriptor.name ] 
            ]
        , g [ transform [ Translate padding 0 ] ]
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


        unterOberIndikator : Float
        unterOberIndikator = (sin (toFloat index * winkel) * radiusUmkreis)

-- Berechnet die Korrelation und rundet sie auf die 4 Nachkommastelle auf. 
        größe :   Float
        größe = runden (Maybe.withDefault 0.0 ( (r (combineLists xAttribut.data datenwerte.data))))     
          
        radiusScale : ContinuousScale Float
        radiusScale  =
            Scale.linear ( 20 , 50 ) (0, 1)

        scaledRadius : Float
        scaledRadius = if größe <= 0.0000001
                then   Scale.convert radiusScale (größe * -1)
                else  Scale.convert radiusScale größe
           
        colorTon : Color
        colorTon = if größe <= 0.0 
                then  redsInterpolator( (größe* -1))
                else greensInterpolator größe      

    in
    g [  ]
        [line xa ya xPosition yPosition
            ,circle [ cx xPosition
                  , cy yPosition
                  , TypedSvg.Attributes.InPx.r (scaledRadius)
                  , fill <| Paint <| Color.rgb 255 255 255
                  , stroke <| Paint <| Color.rgb 0 0 0
                  ]
                   []
            ,circle [ cx xPosition
                  , cy yPosition
                  , TypedSvg.Attributes.InPx.r (scaledRadius)
                  , fill <| Paint <| colorTon
                  , stroke <| Paint <| Color.rgb 0 0 0
                  ]
                   []
        , text_
            [ x (xPosition  )
            , y (if unterOberIndikator <= 0.1 then (yPosition - scaledRadius - 8 )
                 else (yPosition + scaledRadius + 16 + 8))
            , textAnchor TypedSvg.Types.AnchorMiddle
            , TypedSvg.Attributes.class ["hoverText"]
            ]
            [ TypedSvg.Core.text (datenwerte.name  ++ ": "++ String.fromFloat größe) ]
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


