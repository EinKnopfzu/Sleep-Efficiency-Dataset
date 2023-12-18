module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Http
import Color
import Csv 
import Csv.Decode 
import Scatterplot

-- import



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "https://raw.githubusercontent.com/EinKnopfzu/Sleep-Efficiency-Dataset/main/Sleep_Efficiency.csv"
      , expect = Http.expectString GotText
      }
  )


header : Html msg
header =
    div
        [ Html.Attributes.style "background-color" "#333"
        , Html.Attributes.style "color" "white"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "padding" "1em"
        ]
        [ h1 [] [ text "Willkommen auf meiner Seite" ] ]

inhalt : Html msg
inhalt =
    div [ style "padding" "2em",
          style "max-width" "600px",
          style "margin" "0 auto" ]
        [ p [] [ text "Zielstellung : Visualiserung" ]
        , p [] [ text "Hier ist etwas Beispieltext." ]
        ]


footer : Html msg
footer =
    div [ style "background-color" "#333",
          style "color" "white",
          style "text-align" "center",
          style "padding" "1em",
          style "position" "fixed",
          style "bottom" "0", 
          style "width" "100%" ]
        [ p [] [ text "© 2023 Mick Wörner" ]
        ]





-- UPDATE


type Msg
  = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "Ich konnte die Daten nicht laden"

    Loading ->
      text "Am Laden..."

    Success fullText ->
      --  pre [] [ text fullText ]
      div []
        [ header
        , inhalt
        , footer
        ]


--Dieser Bereich ist zum Aufarbeiten der Daten 



csvString_to_data : String -> List ( Unverarbeitete_Daten )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decode_CVD_Data
        |> Result.toMaybe
        |> Maybe.withDefault []

decode_CVD_Data : Csv.Decode.Decoder ((Unverarbeitete_Daten)-> a) a
decode_CVD_Data =
    Csv.Decode.map Unverarbeitete_Daten
        (Csv.Decode.field "ID" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "Age" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Gender" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Bedtime" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Wakeup_time" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Sleep_duration" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Sleep_efficiency" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "REM_sleep_percentage" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Deep_sleep_percentage" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Light_sleep_percentage" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Awakenings" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Caffeine consumption" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Alcohol consumption" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Smoking status" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Exercise frequency"  (String.toFloat >> Result.fromMaybe "error parsing string"))

        )

type alias Unverarbeitete_Daten =  
 {id_ : String 
 ,alter :  Float
 ,geschlecht : String
 ,schlafenszeitt : String
 ,aufwachzeit : String
 ,schlafdauer :  Float
 ,schlaf_effizienz :  Float
 ,rem_anteil :  Float
 ,tiefschlaf_anteil :  Float
 ,leichtschlaf_anteil :  Float
 ,erwacht_anzahl :  Float
 ,koffein_konsum :  Float
 ,alkohol_konsum :  Float
 ,raucher :  String
 ,sport :  Float

  }
--Ziel der Datentransformation  
type alias Daten_angepasst =  
 {id_ : String 
 ,alter : Maybe Float
 ,geschlecht : Bool
 ,schlafenszeitt : String
 ,aufwachzeit : String
 ,schlafdauer : Maybe Float
 ,schlaf_effizienz : Maybe Float
 ,rem_anteil : Maybe Float
 ,tiefschlaf_anteil : Maybe Float
 ,leichtschlaf_anteil : Maybe Float
 ,erwacht_anzahl : Maybe Float
 ,koffein_konsum : Maybe Float
 ,alkohol_konsum : Maybe Float
 ,raucher :  Bool
 ,sport : Maybe Float
 }




type Geschlecht 
  = Male
  | Female


--Dieser Bereich ist für das Vorbereiten der Daten für den Scatterplott

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

