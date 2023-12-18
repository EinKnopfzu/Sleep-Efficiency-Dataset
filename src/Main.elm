module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Http
import Color

import Csv.Decode as Decode exposing (Decoder, decodeCsv)
import Debug exposing (toString)

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
                    div []
                        [ pre [] [ text (toString (Decode.decodeCsv Decode.FieldNamesFromFirstRow decode fullText ))]
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

type alias Aussortierte_Daten =
{id_ :  String 
 ,alter : Float
 ,geschlecht : Geschlecht
 ,schlafenszeitt : String
 ,aufwachzeit : String
 ,schlafdauer : Float
 ,schlaf_effizienz : Float
 ,rem_anteil : Float
 ,tiefschlaf_anteil : Float
 ,leichtschlaf_anteil : Float
 ,erwacht_anzahl : Float
 ,koffein_konsum : Float
 ,alkohol_konsum : Float
 ,raucher : Raucher
 ,sport : Float
 }

type Geschlecht 
  = Male
  | Female

type Raucher
  = Yes
  | No

--Dieser Bereich ist für das Vorbereiten der Daten für den Scatterplott.



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

