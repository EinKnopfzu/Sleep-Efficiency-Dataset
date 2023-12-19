module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (onInput)
import Http
import Color
import List exposing (filter, sum)
import Csv.Decode as Decode exposing (Decoder)
import Debug exposing (toString)
import TypedSvg.Types exposing (YesNo(..))


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
     , daten :List (Maybe Aussortierte_Daten)
 }


type alias RecordName =
    { key1 : Float
    , key2 : Float
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( {datenladen = Loading,
    droppdown1 = "",
    droppdown2 = "",
    droppdown3 = "",
    droppdown4 = ""
      , daten = [ Just { myid_ = "Test"
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
         (({ model | datenladen = Success , daten = ((stringtoUnverarbeitete fullText )|> List.map sleep2Point)},Cmd.none))

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
            text "Ich konnte die Daten nicht laden"

        Loading ->
            text "Am Laden..."

        Success  ->
           Html.div []
            [ Html.div [style "background-color" "#333",
             style "color" "white",
          style "text-align" "center",
          style "padding" "1em",
          style "position" "fixed",
          style "bottom" "0", 
          style "width" "100%"]
               [Html.select [ onInput Option1Selected ]
                [ Html.option [ value "", selected ("" == model.droppdown1) ] [ Html.text "Select an option" ]
               , Html.option [ value "City MPG", selected ("City MPG" == model.droppdown1) ] [ Html.text "City MPG" ]
               , Html.option [ value "Retail Price", selected ("Retail Price" == model.droppdown1) ] [ Html.text "Retail Price" ]
               , Html.option [ value "Dealer Cost", selected ("Dealer Cost" == model.droppdown1) ] [ Html.text "Dealer Cost" ]
               , Html.option [ value "Car Len", selected ("Car Len" == model.droppdown1) ] [ Html.text "Car Len" ]
               , Html.option [ value "Weight", selected ("Weight" == model.droppdown1) ] [ Html.text "Weight" ]
               , Html.option [ value "Car Width", selected ("Car Width" == model.droppdown1) ] [ Html.text "Car Width" ]
               , Html.option [ value "Engine Size", selected ("Engine Size" == model.droppdown1) ] [ Html.text "Engine Size" ]
                
                , text model.droppdown1]]
             , Html.div []
               [ Html.select [ onInput Option2Selected ]
               [ Html.option [ value "", selected ("" == model.droppdown2) ] [ Html.text "Select an option" ]
               , Html.option [ value "City MPG", selected ("City MPG" == model.droppdown2) ] [ Html.text "City MPG" ]
               , Html.option [ value "Retail Price", selected ("Retail Price" == model.droppdown2) ] [ Html.text "Retail Price" ]
               , Html.option [ value "Dealer Cost", selected ("Dealer Cost" == model.droppdown2) ] [ Html.text "Dealer Cost" ]
               , Html.option [ value "Car Len", selected ("Car Len" == model.droppdown2) ] [ Html.text "Car Len" ]
               , Html.option [ value "Weight", selected ("Weight" == model.droppdown2) ] [ Html.text "Weight" ]
               , Html.option [ value "Car Width", selected ("Car Width" == model.droppdown2) ] [ Html.text "Car Width" ]
               , Html.option [ value "Engine Size", selected ("Engine Size" == model.droppdown2) ] [ Html.text "Engine Size" ]
               ], text model.droppdown2] ]  
                 
                 
                 --   div []
                 --       [ header,
                 --         pre [] [ text (toString  model.daten)]
                 --         ,div [] [text "Daten"]
                 --         ,footer
                 --        ]


testwert: Int
testwert = 4000000000

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


  
--Dieser Bereich ist für das Vorbereiten der Daten für den Scatterplott.


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


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

