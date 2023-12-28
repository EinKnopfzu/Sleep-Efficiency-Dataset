module Scatterplot exposing (..)


import Axis
import Browser
import Color exposing (Color(..), rgb)
import Html exposing (text)
import Scale exposing (ContinuousScale)
import Statistics exposing ( quantile)
import TypedSvg exposing (circle, g, line,  svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r,  x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Round exposing (round)


type alias ScatterplottXYPoint =
    { xValue : Float
     ,yValue : Float}

-- XyData soll den Datensatz Beschreiben, der im Scatterplott angezeigt wird
type alias ScatterplottXYData =
    { xDescription : String
    , yDescription : String
    , data : List ScatterplottPoint
    }

--Repräsentiert den individuellen Punkt mit Name. 
type alias ScatterplottPoint =
    { pointName : String, x : Float, y : Float }


--Variablen zur Dartsellung der Größe dr Grafiken gelten auch in den anderen Modulen
w : Float
w =
    900


h : Float
h =
    600


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

combineLists : List Float -> List Float ->List String -> List ScatterplottPoint
combineLists x y z =
    List.map3 (\zValue xValue yValue -> { pointName = zValue, x = xValue, y = yValue }) z x y


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    values
    |> Statistics.extent
    |> Maybe.withDefault defaultExtent


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)
 

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)



addNV : List (ScatterplottPoint)  -> List (ScatterplottPoint)
addNV data  =

    let
        geordneteDaten : List (ScatterplottPoint)
        geordneteDaten =
            List.sortBy .y data

      
        n1 : Float
        n1 =
            List.length geordneteDaten |> toFloat

      
        listederFValuesundDaten : List (ScatterplottPoint)  
        listederFValuesundDaten =
            List.indexedMap (\i pt -> { pt | x = ((toFloat i + 0.5) / n1), y = pt.y }) geordneteDaten
             
        quantilesofData : List (Float)    
        quantilesofData = 
            List.map .x listederFValuesundDaten
        
        valuesofdata : List(Float)
        valuesofdata = 
            List.map .y geordneteDaten
             
                -- Normal Distribution --
        
        transformierer : Float -> Float    
        transformierer f= 
            let 
               mun = List.sum valuesofdata / (toFloat <| List.length valuesofdata)
               stdn = Statistics.deviation valuesofdata |> Maybe.withDefault 0 
                    
            in 
              f * stdn + mun 
            
        invNormalCdf : Float -> Float
        invNormalCdf f =
             if f >= 0.5 then
                5.5556 * (1.0 - ((1.0 - f) / f) ^ 0.1186)
             else
                -5.5556 * (1.0 - (f / (1.0 - f)) ^ 0.1186)    

            
        mu = List.sum valuesofdata / (toFloat <| List.length valuesofdata)
        
        std = Statistics.deviation valuesofdata |> Maybe.withDefault 0 
        
                    
        quantilesWithEstimates : Int -> List Float
        quantilesWithEstimates n = 
         let
             step = 1.0 / (toFloat n - 1.0)
             indexes = List.range 0 (n - 1)
             quantiles = List.map (\i -> mu + std * invNormalCdf (toFloat i * step)) indexes
         in
             List.map (\q -> (q)) quantiles
                
 
        nvFWerte =
           List.map invNormalCdf quantilesofData  


        nvWerte =
            List.map transformierer nvFWerte      


        qqquants = List.map (\a -> Maybe.withDefault 0.0 ( quantile a valuesofdata)) 

        normalverteileDaten : List (Float)
        normalverteileDaten =
            List.map transformierer quantilesofData


        qqPlot = 
            List.map2 (ScatterplottPoint "")  nvWerte valuesofdata
        


    in
     qqPlot

scatterplot : ScatterplottXYData -> Svg msg
scatterplot model =

    let
        {- hier können Sie die Beschriftung des Testpunkts berechnen -}
        kreisbeschriftung : List(String)
        kreisbeschriftung =
            List.map .pointName model.data 

        minXAchse : Float
        minXAchse = 
            Maybe.withDefault 0.0 (List.minimum <| List.map .y model.data)

        maxXAchse : Float
        maxXAchse = 
          Maybe.withDefault 0.0 (List.maximum <| List.map .y model.data)

        minYAchse : Float
        minYAchse = 
            Maybe.withDefault 0.0 (List.minimum <| List.map .x model.data)

        maxYAchse : Float
        maxYAchse = 
          Maybe.withDefault 0.0 (List.maximum <| List.map .x model.data)

        xNVS = Scale.convert xScaleLocal minXAchse
        yNVS= Scale.convert yScaleLocal minXAchse
        xNVE = Scale.convert xScaleLocal maxXAchse
        yNVE = Scale.convert yScaleLocal maxXAchse


        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues
 
        nvWerte =
           List.map .x model.data
           
        valuesofdata =
           List.map .y model.data
           
        nvWerte25 : Float
        nvWerte25 =
            Maybe.withDefault 0.0 (quantile 0.25 nvWerte)
        nvWerte75 : Float
        nvWerte75 =
            Maybe.withDefault 0.0 (quantile 0.75 nvWerte)
        valuesofdata25 =
          Maybe.withDefault 0.0 (quantile 0.25 valuesofdata)
        valuesofdata75 =
            Maybe.withDefault 0.0 (quantile 0.75 valuesofdata)

        xS = Scale.convert xScaleLocal nvWerte25
        yS= Scale.convert yScaleLocal valuesofdata25
        xE = Scale.convert xScaleLocal nvWerte75
        yE = Scale.convert yScaleLocal valuesofdata75

               
    in
    svg [ viewBox 0 0 (w)  (h ) , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 80 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 255, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0,  0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
    
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        , g
            [ transform [ Translate padding (h- padding) ]
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
            [ transform [ Translate  padding padding ]
            , class [ "y-axis" ]
            ]
            [ yAxis yValues
            , text_
                [ x 0
                , y -15
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ Html.text model.yDescription ]
            ]
        
        , g [ transform [ Translate padding padding ] ]
            [ line
              [ TypedSvg.Attributes.InPx.x1 (xS)
                , TypedSvg.Attributes.InPx.y1 (yS)
                , TypedSvg.Attributes.InPx.x2 (xE)
                , TypedSvg.Attributes.InPx.y2 (yE)
                , TypedSvg.Attributes.InPx.strokeWidth 2  
                , stroke <| Paint <| Color.rgba 255 0 0 1
                ]    []
            ] 
        ,  g [ transform [ Translate padding padding ] ]
            [ line
              [ TypedSvg.Attributes.InPx.x1 (xNVS)
                , TypedSvg.Attributes.InPx.y1 (yNVS)
                , TypedSvg.Attributes.InPx.x2 (xNVE)
                , TypedSvg.Attributes.InPx.y2 (yNVE)
                , TypedSvg.Attributes.InPx.strokeWidth 1  
                , stroke <| Paint <| Color.rgba 0 0 0 0.9
                ]    []
            ] 
        , g [ transform [ Translate padding padding ] ]
            [ text_
                [ x ( 400)
                , y (0)
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ Html.text ("Minimum:" ++ " "++ (Round.ceiling 3 minXAchse )++ "  " ++ " Maximum: " ++" " ++ (Round.ceiling  3 maxXAchse )++ " ")] 
            ]
            ]
        

point : ContinuousScale Float -> ContinuousScale Float -> ScatterplottPoint -> Svg msg
point scaleX scaleY xyPoint =
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




