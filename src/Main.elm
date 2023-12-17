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
      text "I was unable to load the data :("

    Loading ->
      text "Loading..."

    Success fullText ->
       pre [] [ text fullText ]

-- DATA
--This area is for preparing and manipulating the data: Each step has to be named and the purpose described




csvString_to_data : String -> List ( Unprocessed_CVD_Data )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decode_CVD_Data
        |> Result.toMaybe
        |> Maybe.withDefault []

decode_CVD_Data : Csv.Decode.Decoder ((Unprocessed_CVD_Data)-> a) a
decode_CVD_Data =
    Csv.Decode.map Unprocessed_CVD_Data
        (Csv.Decode.field "general_Health" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "checkup" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "exercise" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "heart_Disease" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "skin_Cancer" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "other_Cancer" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "depression" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "diabetes" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "arthritis" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "sex" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "age_Category" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "height" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "weight" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "bmi" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "smoking_History" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "alcohol"  (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "fruit"  (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "green_Vegetables" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "friedPotato" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )

type alias Unprocessed_CVD_Data = 
 {general_Health : String 
 ,checkup : String
 ,exercise : String
 ,heart_Disease : String
 ,skin_Cancer : String
 ,other_Cancer : String
 ,depression : String
 ,diabetes : String
 ,arthritis : String
 ,sex : String
 ,age_Category : String
 ,height : Float
 ,weight : Float
 ,bmi :  Float
 ,smoking_History : String
 ,alcohol : Float
 ,fruit : Float
 ,green_Vegetables : Float
 ,friedPotato : Float
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

type Sex 
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
 
