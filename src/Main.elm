module Main exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Http
import Color
import Csv 
import Csv.Decode
import Html exposing (a)
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
      { url = "https://mick.woerner.gpages-lehre.informatik.uni-halle.de/informationsvisualisierung-cardiovascular-diseases-risk-prediction-dataset/CVD_cleaned.csv"
      , expect = Http.expectString GotText
      }
  )



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
       pre [] [ text fullText ]

--Dieser Bereich ist zum Analysieren und Manipulieren von Daten 



csvString_to_data : String -> List ( Unprocessed_CVD_Data )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decode_CVD_Data
        |> Result.toMaybe
        |> Maybe.withDefault []

decode_CVD_Data : Csv.Decode.Decoder ((Unprocessed_CVD_Data)-> a) a
decode_CVD_Data =
    Csv.Decode.map Unprocessed_CVD_Data
        (Csv.Decode.field "ID" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "Age" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Gender" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Bedtime" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Wakeup_time" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Sleep_duration" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Sleep_efficiency" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "REM_sleep_percentage" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Deep_sleep_percentage" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Light_sleep_percentage" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Awakenings" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Caffeine consumption" Ok)--(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Alcohol consumption" Ok)--(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Smoking status" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Exercise frequency"  Ok)--(String.toFloat >> Result.fromMaybe "error parsing string"))

        )

type alias Unprocessed_CVD_Data =  
 {id_ : String 
 ,alter : String
 ,geschlecht : String
 ,schlafenszeitt : String
 ,aufwachzeit : String
 ,schlafdauer : String
 ,schlaf_effizienz : String
 ,rem_anteil : String
 ,tiefschlaf_anteil : String
 ,leichtschlaf_anteil : String
 ,erwacht_anzahl : String
 ,koffein_konsum : String
 ,alkohol_konsum : String
 ,raucher :  String
 ,sport : String

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

type alias CVD_Data_adapted = 
 { general_Health : Health
 ,checkup : CheckupHealth 
 ,exercise : Bool
 ,heart_Disease : Bool
 , skin_Cancer : Bool
 , other_Cancer : Bool
 ,depression : Bool
 ,diabetes : Bool
 ,arthritis : Bool
 ,sex : Sex
 ,age_Category : String
 ,height : Maybe Float
 ,weight : Maybe Float
 ,bmi : Maybe Float
 ,smoking_History : String
 ,alcohol : Maybe Float
 ,fruit : Maybe Float
 ,green_Vegetables : Maybe Float
 ,friedPotato : Maybe Float
 }

type Health 
  = Very_Good
  | Poor
  | Fair
  | Good
  | Excellent

type Geschlecht 
  = Male
  | Female

type Binär 
  = Yes
  | No 

type CheckupHealth
 = Within_the_past_year
 | Within_the_past_2_years
 | Whithin_the_past_5_years
 | Five_or_more_years_ago
 | Never 
 
