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
import Scatterplot exposing (..)
import Boxplott exposing (..)


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
          Html.Attributes.style "max-width" "900px",
          Html.Attributes.style "margin" "0 auto" ]
        [ p [] [ Html.text "Zielstellung : Visualiserung" ]
        , p [] [ Html.text "Hier ist etwas Beispieltext." ]
        ]

footer : Html msg
footer =
    div [ Html.Attributes.style "background-color" "#333",
          Html.Attributes.style "color" "white",
          Html.Attributes.style "text-align" "center",
          Html.Attributes.style "padding" "0em",
          Html.Attributes.style "position" "fixed",
          Html.Attributes.style "bottom" "0", 
          Html.Attributes.style "width" "100%",
          Html.Attributes.style "height" "10px" ]
        [ p [] [ Html.text "© 2023 Mick Wörner 217246242" ]
        ]


type Zustand
  = Failure
  | Loading
  | Success 

type PageState
  = Scatterplott
  | Grafik1
  | Grafik2


type alias Model
 = { datenladen : Zustand
     , droppdown1 : String
     , droppdown2 : String
     , droppdown3 : String
     , droppdown4 : String
     , daten :List (Aussortierte_Daten)
     , pageview : PageState
 }



init : () -> (Model, Cmd Msg)
init _ =
  ( 
      {datenladen = Loading,
      droppdown1 = "",
      droppdown2 = "",
      droppdown3 = "",
      droppdown4 = "",
      pageview = Scatterplott
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
  | PageChange PageState
  


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ({ model | datenladen = Success, daten = ((stringtoUnverarbeitete fullText) |> List.filterMap sleep2Point) }, Cmd.none)

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

        PageChange value ->
            case value of
                Scatterplott ->
                    ({ model | pageview = Scatterplott} , Cmd.none)
                Grafik1 ->
                    ({ model | pageview = Grafik1} , Cmd.none)
                Grafik2 ->
                    ({ model | pageview = Grafik2} , Cmd.none)

        

      



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
            Html.div[]
            [ Html.div [][header],
            Html.text "Am Laden..."]

        Success  ->
            Html.div [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "height" "100vh" ]
                 [ header
                  , Html.div [ Html.Attributes.style "display" "flex" ]
                [ Html.div 
                   [ Html.Attributes.style "background-color" "#333"
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "1em"
                    , Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.style "width" "10%"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "font-family" "Arial"
                  ]
                  [Html.u[][ Html.text " SETTINGS "]
                       , Html.br [] []
                       , Html.br [] []
                       , button [ Html.Events.onClick (PageChange Scatterplott) ] [ Html.text "Scatterplot" ]
                       , Html.br [] []
                       , button [ Html.Events.onClick (PageChange Grafik1) ] [ Html.text "Parallele Koordinaten" ]
                       , Html.br [] []
                       , button [ Html.Events.onClick (PageChange Grafik2) ] [ Html.text "Farbskala" ]
                       , Html.br [] []
                       , Html.br [] []
                       , Html.br [] []
                       , Html.br [] []
                       , Html.br [] []
                       ,Html.text "X-Achse"               
                  ,Html.select [ onInput Option1Selected ]
                [ Html.option [ value "", selected ("" == model.droppdown1) ] [ Html.text "Select an option" ]
                  , Html.option [ value "Alter", selected ("Alter" == model.droppdown1) ] [ Html.text "Alter" ]
                  , Html.option [ value "Schlafdauer", selected ("Schlafdauer" == model.droppdown1) ] [ Html.text "Schlafdauer" ]
                  , Html.option [ value "Schlaf Effizient", selected ("Schlaf Effizient" == model.droppdown1) ] [ Html.text "Schlaf Effizient" ]
                  , Html.option [ value "REM", selected ("REM" == model.droppdown1) ] [ Html.text "REM" ]
                  , Html.option [ value "Tiefschlaf Anteil", selected ("Tiefschlaf Anteil" == model.droppdown1) ] [ Html.text "Tiefschlaf Anteil" ]
                  , Html.option [ value "Erwacht Anzahl", selected ("Erwacht Anzahl" == model.droppdown1) ] [ Html.text "Erwacht Anzahl" ]
                  , Html.option [ value "Koffein Konsum", selected ("Koffein Konsum" == model.droppdown1) ] [ Html.text "Koffein Konsum" ]                
                  ]
                  , Html.br [] []
                  , Html.br [] []
               , Html.div []
                  [ Html.text "Y-Achse" 
                  ,Html.select [ onInput Option2Selected ]
                  [ Html.option [ value "", selected ("" == model.droppdown2) ] [ Html.text "Select an option" ]
                  , Html.option [ value "Alter", selected ("Alter" == model.droppdown2) ] [ Html.text "Alter" ]
                  , Html.option [ value "Schlafdauer", selected ("Schlafdauer" == model.droppdown2) ] [ Html.text "Schlafdauer" ]
                  , Html.option [ value "Schlaf Effizient", selected ("Schlaf Effizient" == model.droppdown2) ] [ Html.text "Schlaf Effizient" ]
                  , Html.option [ value "REM", selected ("REM" == model.droppdown2) ] [ Html.text "REM" ]
                  , Html.option [ value "Tiefschlaf Anteil", selected ("Tiefschlaf Anteil" == model.droppdown2) ] [ Html.text "Tiefschlaf Anteil" ]
                  , Html.option [ value "Erwacht Anzahl", selected ("Erwacht Anzahl" == model.droppdown2) ] [ Html.text "Erwacht Anzahl" ]
                  , Html.option [ value "Koffein Konsum", selected ("Koffein Konsum" == model.droppdown2) ] [ Html.text "Koffein Konsum" ]]
                  ]
                  , Html.br [] []
                  
               , Html.div []
                  [ Html.text "Z-Achse" 
                  ,Html.select [ onInput Option3Selected ]
                  [ Html.option [ value "", selected ("" == model.droppdown3) ] [ Html.text "Select an option" ]
                  , Html.option [ value "Alter", selected ("Alter" == model.droppdown3) ] [ Html.text "Alter" ]
                  , Html.option [ value "Schlafdauer", selected ("Schlafdauer" == model.droppdown3) ] [ Html.text "Schlafdauer" ]
                  , Html.option [ value "Schlaf Effizient", selected ("Schlaf Effizient" == model.droppdown3) ] [ Html.text "Schlaf Effizient" ]
                  , Html.option [ value "REM", selected ("REM" == model.droppdown3) ] [ Html.text "REM" ]
                  , Html.option [ value "Tiefschlaf Anteil", selected ("Tiefschlaf Anteil" == model.droppdown3) ] [ Html.text "Tiefschlaf Anteil" ]
                  , Html.option [ value "Erwacht Anzahl", selected ("Erwacht Anzahl" == model.droppdown3) ] [ Html.text "Erwacht Anzahl" ]
                  , Html.option [ value "Koffein Konsum", selected ("Koffein Konsum" == model.droppdown3) ] [ Html.text "Koffein Konsum" ]]
               
               , Html.text model.droppdown3]
                                 , Html.br [] []
                  
               , Html.div []
                  [ Html.text "E-Achse" 
                  ,Html.select [ onInput Option4Selected ]
                  [ Html.option [ value "", selected ("" == model.droppdown4) ] [ Html.text "Select an option" ]
                  , Html.option [ value "Alter", selected ("Alter" == model.droppdown4) ] [ Html.text "Alter" ]
                  , Html.option [ value "Schlafdauer", selected ("Schlafdauer" == model.droppdown4) ] [ Html.text "Schlafdauer" ]
                  , Html.option [ value "Schlaf Effizient", selected ("Schlaf Effizient" == model.droppdown4) ] [ Html.text "Schlaf Effizient" ]
                  , Html.option [ value "REM", selected ("REM" == model.droppdown4) ] [ Html.text "REM" ]
                  , Html.option [ value "Tiefschlaf Anteil", selected ("Tiefschlaf Anteil" == model.droppdown4) ] [ Html.text "Tiefschlaf Anteil" ]
                  , Html.option [ value "Erwacht Anzahl", selected ("Erwacht Anzahl" == model.droppdown4) ] [ Html.text "Erwacht Anzahl" ]
                  , Html.option [ value "Koffein Konsum", selected ("Koffein Konsum" == model.droppdown4) ] [ Html.text "Koffein Konsum" ]]
               
               , Html.text model.droppdown3]
               
               ]
               ]
          --    ,Html.div [] [Html.text (toString model.daten)]
               , let
                   xList : List (Float)
                   xList =
                     case model.droppdown1 of

                      "Alter" -> List.map .myalter model.daten

                      "Schlafdauer" -> List.map .myschlafdauer model.daten

                      "Schlaf Effizient" -> List.map  .myschlaf_effizienz model.daten

                      "REM" -> List.map .myrem_anteil model.daten

                      "Tiefschlaf Anteil" ->  List.map  .mytiefschlaf_anteil model.daten

                      "Erwacht Anzahl" -> List.map .myleichtschlaf_anteil model.daten

                      "Koffein Konsum" -> List.map .myerwacht_anzahl model.daten

                      _ -> []    
     
                   yList : List (Float)
                   yList =
                      case model.droppdown2 of

                      "Alter" -> List.map .myalter model.daten

                      "Schlafdauer" -> List.map .myschlafdauer model.daten

                      "Schlaf Effizient" -> List.map  .myschlaf_effizienz model.daten

                      "REM" -> List.map .myrem_anteil model.daten

                      "Tiefschlaf Anteil" ->  List.map  .mytiefschlaf_anteil model.daten

                      "Erwacht Anzahl" -> List.map .myerwacht_anzahl model.daten

                      "Koffein Konsum" -> List.map .mykoffein_konsum model.daten
                      
                      _ -> [] 

                   zList : List (Float)
                   zList =
                      case model.droppdown3 of

                      "Alter" -> List.map .myalter model.daten

                      "Schlafdauer" -> List.map .myschlafdauer model.daten

                      "Schlaf Effizient" -> List.map  .myschlaf_effizienz model.daten

                      "REM" -> List.map .myrem_anteil model.daten

                      "Tiefschlaf Anteil" ->  List.map  .mytiefschlaf_anteil model.daten

                      "Erwacht Anzahl" -> List.map .myerwacht_anzahl model.daten

                      "Koffein Konsum" -> List.map .mykoffein_konsum model.daten
                      
                      _ -> [] 
                   name =
                    List.map .myid_ model.daten

                   combinedList_Scatter: List ScatterplottPoint 
                   combinedList_Scatter =
                              (combineLists xList xList name) |> addNV

--Hier werden die Listen in für den Boxplott vorbereitet in diesem Fall istes wichtig, dass die XListe 
--Die Mittlere Liste ist, da wir ansonsten nicht das zu suchende element in der Mitte haben. 
                   combinedList_Box : List MultiDimPoint
                   combinedList_Box = Boxplott.combineLists_Box name  yList xList zList 

                   
                in
                   div [
                    Html.Attributes.style "margin-left" "20%" 
                    , Html.Attributes.style "padding" "2em"
                    , Html.Attributes.style "height" "700" 
                    , Html.Attributes.style "width" "600"
                    , Html.Attributes.style "font-family" "Arial"] 
                    [ scatterplot 
                    { xDescription = "Normalverteilung"
                    , yDescription = model.droppdown2
                    , data = combinedList_Scatter
                    }
                    ,boxplott combinedList_Box]

                    
                   , footer
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

type alias MultiDimPoint =
    { pointName : String, value : List Float }

 
--Dieser Bereich ist für das Vorbereiten der Daten für den Scatterplott.