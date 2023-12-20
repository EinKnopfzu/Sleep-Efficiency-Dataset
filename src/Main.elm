module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (onInput)
import Maybe.Extra exposing (isJust)
import Http
import Color
import List exposing (filter, sum)
import Csv.Decode as Decode exposing (Decoder)
import Debug exposing (toString)
import Scale exposing (ContinuousScale)
import Axis
import Stat
import Statistics exposing (extent, quantile)
import TypedSvg exposing (circle, g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, color, fill, fontFamily, fontSize, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..),YesNo(..))


-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL
header : Html msg
header =
    div
        [ Html.Attributes.style "background-color" "#333"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "padding" "0.5em"
        ]
        [ h1 [] [ Html.text "Informationsvisualiserung Sleep Quality" ] ]

inhalt : Html msg
inhalt =
    div [ Html.Attributes.style "padding" "2em",
          Html.Attributes.style "max-width" "600px",
          Html.Attributes.style "margin" "0 auto" ]
        [ p [] [ Html.text "Zielstellung : Visualiserung" ]
        , p [] [ Html.text "Hier ist etwas Beispieltext." ]
        ]

footer : Html msg
footer =
    div [ Html.Attributes.style "background-color" "#333",
          Html.Attributes.style "color" "white",
          Html.Attributes.style "text-align" "center",
          Html.Attributes.style "padding" "1em",
          Html.Attributes.style "position" "fixed",
          Html.Attributes.style "bottom" "0", 
          Html.Attributes.style "width" "100%" ]
        [ p [] [ Html.text "© 2023 Mick Wörner 217246242" ]
        ]


type Zustand
  = Failure
  | Loading
  | Success 

type alias Model
 = { datenladen : Zustand
     , droppdown1 : String
     , droppdown2 : String
     , droppdown3 : String
     , droppdown4 : String
     , daten :List ( Aussortierte_Daten)
 }


type alias RecordName =
    { key1 : Float
    , key2 : Float
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( 
      {datenladen = Loading,
      droppdown1 = "",
      droppdown2 = "",
      droppdown3 = "",
      droppdown4 = ""
      , daten = [  { myid_ = "Test"
      , myalter = 0.0
      , mygeschlecht = "NA"
      , myschlafenszeitt = "NA"
      , myaufwachzeit = "NA"
      , myschlafdauer = 0.0
      , myschlaf_effizienz = 0.0
      , myrem_anteil = 0.0
      , mytiefschlaf_anteil = 0.0
      , myleichtschlaf_anteil = 0.0
      , myerwacht_anzahl = 0.0
      , mykoffein_konsum = 0.0
      , myalkohol_konsum = 0.0
      , myraucher = "NA"
      , mysport = 0.0
                       } ]
      }
  , Http.get
      { url = "https://raw.githubusercontent.com/EinKnopfzu/Sleep-Efficiency-Dataset/main/Sleep_Efficiency.csv"
      , expect = Http.expectString GotText
      }
  )

-- UPDATE

type Msg
  = GotText (Result Http.Error String)
  | Option1Selected String
  | Option2Selected String
  | Option3Selected String
  | Option4Selected String
  


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
         (({ model | datenladen = Success , daten = ((stringtoUnverarbeitete fullText )|> List.filterMap sleep2Point)},Cmd.none))

        Err _ ->
         ({ model | datenladen = Failure }, Cmd.none)

    Option1Selected value -> 
      ({ model | droppdown1 = value }, Cmd.none)

    Option2Selected value -> 
      ({ model | droppdown2 = value }, Cmd.none)

    Option3Selected value ->
      ({ model | droppdown3 = value }, Cmd.none)

    Option4Selected value -> 
      ({ model | droppdown4 = value }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    case model.datenladen of
        Failure ->
            Html.text "Ich konnte die Daten nicht laden"

        Loading ->
            Html.text "Am Laden..."

        Success  ->
           Html.div []
            [ Html.div 
              [Html.Attributes.style "background-color" "#333"
              , Html.Attributes.style "color" "white"
              , Html.Attributes.style "text-align" "center"
              , Html.Attributes.style "padding" "1em"
              , Html.Attributes.style "position" "fixed"
              , Html.Attributes.style "left" "0"
              , Html.Attributes.style "width" "10%"
              , Html.Attributes.style "height" "100%"]
               [Html.text "Erster Daten Wert:"               
               ,Html.select [ onInput Option1Selected ]
                [ Html.option [ value "", selected ("" == model.droppdown1) ] [ Html.text "Select an option" ]
               , Html.option [ value "City MPG", selected ("City MPG" == model.droppdown1) ] [ Html.text "City MPG" ]
               , Html.option [ value "Retail Price", selected ("Retail Price" == model.droppdown1) ] [ Html.text "Retail Price" ]
               , Html.option [ value "Dealer Cost", selected ("Dealer Cost" == model.droppdown1) ] [ Html.text "Dealer Cost" ]
               , Html.option [ value "Car Len", selected ("Car Len" == model.droppdown1) ] [ Html.text "Car Len" ]
               , Html.option [ value "Weight", selected ("Weight" == model.droppdown1) ] [ Html.text "Weight" ]
               , Html.option [ value "Car Width", selected ("Car Width" == model.droppdown1) ] [ Html.text "Car Width" ]
               , Html.option [ value "Engine Size", selected ("Engine Size" == model.droppdown1) ] [ Html.text "Engine Size" ]                
               , Html.text model.droppdown1]
             , Html.div []
               [ Html.text "Erster Daten Wert" 
               ,Html.select [ onInput Option2Selected ]
               [ Html.option [ value "", selected ("" == model.droppdown2) ] [ Html.text "Select an option" ]
               , Html.option [ value " MPG", selected ("City MPG" == model.droppdown2) ] [ Html.text "City MPG" ]
               , Html.option [ value "Retail Price", selected ("Retail Price" == model.droppdown2) ] [ Html.text "Retail Price" ]
               , Html.option [ value "Dealer Cost", selected ("Dealer Cost" == model.droppdown2) ] [ Html.text "Dealer Cost" ]
               , Html.option [ value "Car Len", selected ("Car Len" == model.droppdown2) ] [ Html.text "Car Len" ]
               , Html.option [ value "Weight", selected ("Weight" == model.droppdown2) ] [ Html.text "Weight" ]
               , Html.option [ value "Car Width", selected ("Car Width" == model.droppdown2) ] [ Html.text "Car Width" ]
               , Html.option [ value "Engine Size", selected ("Engine Size" == model.droppdown2) ] [ Html.text "Engine Size" ]]
               
               , Html.text model.droppdown2]
               ]
          --    ,Html.div [] [Html.text (toString model.daten)]
               , let
                   xList : List (Float)
                   xList =
                     case model.droppdown1 of

                      "City MPG" -> List.map .mykoffein_konsum model.daten

                      "myretailPrice" -> List.map .myschlafdauer model.daten

                      "Dealer Cost" -> List.map  .myschlaf_effizienz model.daten

                      "Car Len" -> List.map .myrem_anteil model.daten

                      "Weight" ->  List.map  .mytiefschlaf_anteil model.daten

                      "Car Width" -> List.map .myleichtschlaf_anteil model.daten

                      "Engine Size" -> List.map .myerwacht_anzahl model.daten

                      _ -> []    
     
                   yList : List (Float)
                   yList =
                      case model.droppdown2 of

                      "City MPG" -> List.map .mykoffein_konsum model.daten

                      "myretailPrice" -> List.map .myschlafdauer model.daten

                      "Dealer Cost" -> List.map  .myschlaf_effizienz model.daten

                      "Car Len" -> List.map .myrem_anteil model.daten

                      "Weight" ->  List.map  .mytiefschlaf_anteil model.daten

                      "Car Width" -> List.map .myleichtschlaf_anteil model.daten

                      "Engine Size" -> List.map .myerwacht_anzahl model.daten
                      
                      _ -> [] 
                   name =
                    List.map .myid_ model.daten

                   combinedList: List ScatterplottPoint 
                   combinedList =
                             combineLists xList yList name
                in
                   div [
                    Html.Attributes.style "margin-left" "20%" 
                    , Html.Attributes.style "padding" "2em"
                    ,  Html.Attributes.style "height" "100%" 
                    , Html.Attributes.style "width" "90%"] 
                    [ scatterplot 
                    { xDescription = model.droppdown1
                    , yDescription = model.droppdown2
                    , data = combinedList
                    }
                  ,Html.text (toString combinedList)
                   ]
                ]

  


--Dieser Bereich ist zum Aufarbeiten der Daten 

decode : Decoder Unverarbeitete_Daten
decode =
    Decode.into Unverarbeitete_Daten
        |> Decode.pipeline (Decode.field "ID" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Age" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Gender" (Decode.blank Decode.string))       
        |> Decode.pipeline (Decode.field "Bedtime" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Wakeup time" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Sleep duration" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Sleep efficiency" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "REM sleep percentage" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Deep sleep percentage" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Light sleep percentage" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Awakenings" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Caffeine consumption" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Alcohol consumption" (Decode.blank Decode.float))
        |> Decode.pipeline (Decode.field "Smoking status" (Decode.blank Decode.string))
        |> Decode.pipeline (Decode.field "Exercise frequency" (Decode.blank Decode.float))

stringtoUnverarbeitete : String -> List (Unverarbeitete_Daten)
stringtoUnverarbeitete string =
    let
        result = Decode.decodeCsv Decode.FieldNamesFromFirstRow decode string
    in
    case result of
    Ok value -> value
    Err msg ->
          []

-- Diese Funktion verwandelt den Datensatz so, dass die leeren Datenfelder entfernt werden. 
sleep2Point : Unverarbeitete_Daten -> Maybe Aussortierte_Daten
sleep2Point c = 
    Maybe.map5
        (\myid_ myalter mygeschlecht myschlafenszeit myaufwachzeit  ->
            Aussortierte_Daten
                (myid_)
                (myalter)
                (mygeschlecht)
                (myschlafenszeit)
                (myaufwachzeit)
                0.0
                0.0
                0.0
                0.0
                0.0
                0.0
                0.0
                0.0
                "NA"
                0.0
        )
        c.id_
        c.alter
        c.geschlecht
        c.schlafenszeitt
        c.aufwachzeit
        |> Maybe.andThen
            (\cX ->
                Maybe.map5
                    (\myXA myXB myXY myXV myXK->
                        { cX | 
                         myschlafdauer =  myXA,
                         myschlaf_effizienz =myXB, 
                         myrem_anteil = myXY,
                         mytiefschlaf_anteil = myXV,
                         myleichtschlaf_anteil = myXK
                          }
                    )
                    c.schlafdauer
                    c.schlaf_effizienz
                    c.rem_anteil
                    c.tiefschlaf_anteil
                    c.leichtschlaf_anteil
            )
        |> Maybe.andThen
            (\cX ->
                Maybe.map5
                    (\myXA myXB myXY myXV myXK->
                        { cX | 
                         myerwacht_anzahl = myXA,
                         mykoffein_konsum =  myXB, 
                         myalkohol_konsum = myXY,
                         myraucher = myXV,
                         mysport = myXK
                          }
                    )
                    c.erwacht_anzahl
                    c.koffein_konsum
                    c.alkohol_konsum
                    c.raucher
                    c.sport
            )

type alias Aussortierte_Daten =
 {myid_ :  String 
 ,myalter : Float
 ,mygeschlecht : String
 ,myschlafenszeitt : String
 ,myaufwachzeit : String
 ,myschlafdauer : Float
 ,myschlaf_effizienz : Float
 ,myrem_anteil : Float
 ,mytiefschlaf_anteil : Float
 ,myleichtschlaf_anteil : Float
 ,myerwacht_anzahl : Float
 ,mykoffein_konsum : Float
 ,myalkohol_konsum : Float
 ,myraucher : String
 ,mysport : Float
 }
-- Typ für die unaufgearbeiteten Daten
type alias Unverarbeitete_Daten =  
 {id_ :  Maybe String 
 ,alter : Maybe Float
 ,geschlecht :  Maybe String
 ,schlafenszeitt :  Maybe String
 ,aufwachzeit :  Maybe String
 ,schlafdauer : Maybe Float
 ,schlaf_effizienz : Maybe Float
 ,rem_anteil : Maybe Float
 ,tiefschlaf_anteil : Maybe Float
 ,leichtschlaf_anteil : Maybe Float
 ,erwacht_anzahl : Maybe Float
 ,koffein_konsum : Maybe Float
 ,alkohol_konsum : Maybe Float
 ,raucher :  Maybe String
 ,sport : Maybe Float
 }



 
--Dieser Bereich ist für das Vorbereiten der Daten für den Scatterplott.
combineLists : List Float -> List Float ->List String -> List ScatterplottPoint
combineLists x y z =
    List.map3 (\zValue xValue yValue -> { pointName = zValue, x = xValue, y = yValue }) z x y

w : Float
w =
    100

h : Float
h =
    100

padding : Float
padding =
    40

radius : Float
radius =
    2.0

tickCount : Int
tickCount =
    5

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

--Der Typ soll die relative Position eines Datenpunktes darstellen. 
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

scatterplot : ScatterplottXYData -> Svg msg
scatterplot model =
    let
        {- hier können Sie die Beschriftung des Testpunkts berechnen -}
        kreisbeschriftung : String
        kreisbeschriftung =
            ""

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
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        , g
            [ transform [ Translate padding (h-padding) ]
            , TypedSvg.Attributes.class [ "x-axis" ]
            ]
            [ xAxis xValues
            , text_
                [ x 4
                , y 4
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g
            [ transform [ Translate padding padding ]
            , TypedSvg.Attributes.class [ "y-axis" ]
            ]
            [ yAxis yValues
            , text_
                [ x 0
                , y -0
                , fontSize (TypedSvg.Types.px 16)
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        , line
            [ x 0
            , y 0
            , x 100
            , y 100
            , stroke (Paint Color.blue)
            ]
            []
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


