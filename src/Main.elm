module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Http
import Color
import List exposing (filter, sum)
import Csv.Decode as Decode exposing (Decoder)
import Debug exposing (toString)
import TypedSvg.Types exposing (YesNo(..))

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
        , Html.Attributes.style "padding" "0.5em"
        ]
        [ h1 [] [ text "Informationsvisualiserung Sleep Quality" ] ]

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
        [ p [] [ text "© 2023 Mick Wörner 217246242" ]
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
                        [ header,
                          pre [] [ text (toString  ((stringtoUnverarbeitete fullText )|> List.map sleep2Point))]
                          ,footer
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


stringtoUnverarbeitete : String -> List(Unverarbeitete_Daten)
stringtoUnverarbeitete string =
    let
        result = Decode.decodeCsv Decode.FieldNamesFromFirstRow decode string
    in
    case result of
    Ok value -> value
    Err msg ->
          []
            
{-                { myid_ = "0"
                , myalter = "0.0"
                , mygeschlecht = "Unknown"
                , myschlafenszeitt = "0"
                , myaufwachzeit = "0"
                , myschlafdauer = 0.0
                , myschlaf_effizienz = 0.0
                , myrem_anteil = 0.0
                , mytiefschlaf_anteil = 0.0
                , myleichtschlaf_anteil = 0.0
                , myerwacht_anzahl = 0.0
                , mykoffein_konsum = 0.0
                , myalkohol_konsum = 0.0
                , myraucher = "Unknown"
                , mysport = 0.0
                }

--["0",0,Unknown,"0","0",0,0,0,0,0,NA,0]



sleepData i = 
  List.filter sleep2Point i

-}

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



type Geschlecht 
  = Male
  | Female
  | Unknown

type Raucher
  = YES
  | NO
  | NA
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

