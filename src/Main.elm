module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (onInput)
import Http
import List exposing (filter, sum)
import Csv.Decode as Decode exposing (Decoder)
import Scatterplot exposing (..)
import ParalleleKoordinatenRoengten exposing (blackbox) 
import ImpactGraph exposing (graph)


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
  --        Html.Attributes.style "position" "fixed",
          Html.Attributes.style "bottom" "0 + padding", 
          Html.Attributes.style "width" "100%",
          Html.Attributes.style "height" "4%" ]
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
     , droppdown5 : String
     , daten :List (Aussortierte_Daten)
     , pageview : PageState
     , minFilter : Maybe Float
     , maxFilter : Maybe Float }



init : () -> (Model, Cmd Msg)
init _ =
  ( 
      {datenladen = Loading,
      droppdown1 = "",
      droppdown2 = "",
      droppdown3 = "",
      droppdown4 = "",
      droppdown5 = "",
      minFilter = Nothing,
      maxFilter = Nothing,
      pageview = Scatterplott
      , daten = [  { myid_ = "Test"
      , myalter = 0.0
      , mygeschlecht = 0.0
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
      , myraucher = 0.0
      , mysport = 0.0
                       }
      ]
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
  | MinFilterChanged String
  | MaxFilterChanged String
  


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

        MinFilterChanged value ->
            ({ model | minFilter =  (String.toFloat value) }, Cmd.none)

        MaxFilterChanged value ->
            ({ model | maxFilter =  (String.toFloat value) }, Cmd.none)

        

      



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
                 [ 
                  Html.div [ Html.Attributes.style "display" "flex" ]
                [ Html.div 
                   [ Html.Attributes.style "background-color" "#333"
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "1em"
                    , Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.style "width" "15%"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "font-family" "Arial"
                  ]
                  [
                                      Html.br [] []
                  , Html.br [] []
                  , Html.br [] []
                  , Html.br [] []
                    , Html.u[Html.Attributes.style "font-size" "20px" 
                         , Html.Attributes.style "font-weight" "bold"]
                         [ Html.text " Einstellungen " ]
                         , Html.br [] []
                         , Html.br [] []
                         , Html.br [] []
                         , Html.u [ Html.Attributes.style "font-size" "16px" ] [Html.text "Norm-QQ Plot"]
                         , Html.br [] []
                         , Html.br [] []
                       , Html.span [ Html.Attributes.style "font-size" "16px" ] [Html.text "Untersuchendes Attribut" ]
                       ,Html.br [] []        
                  ,Html.select [ onInput Option1Selected ]
                [ Html.option [ value "", selected ("" == model.droppdown1) ] [ Html.text "NA" ]
                  , Html.option [ value "Geschlecht", selected ("Geschlecht" == model.droppdown1) ] [ Html.text "Geschlecht Bool" ]
                  , Html.option [ value "Alter", selected ("Alter" == model.droppdown1) ] [ Html.text "Alter" ]
                  , Html.option [ value "Schlafdauer", selected ("Schlafdauer" == model.droppdown1) ] [ Html.text "Schlafdauer" ]
                  , Html.option [ value "Schlaf Effizienz", selected ("Schlaf Effizienz" == model.droppdown1) ] [ Html.text "Schlaf Effizienz" ]
                  , Html.option [ value "REM", selected ("REM" == model.droppdown1) ] [ Html.text "REM" ]
                  , Html.option [ value "Tiefschlaf Anteil", selected ("Tiefschlaf Anteil" == model.droppdown1) ] [ Html.text "Tiefschlaf Anteil" ]
                  , Html.option [ value "Leichtschlaf Anteil" , selected ("Leichtschlaf Anteil" == model.droppdown1) ] [ Html.text "Leichtschlaf Anteil" ]  
                  , Html.option [ value "Erwacht Anzahl", selected ("Erwacht Anzahl" == model.droppdown1) ] [ Html.text "Erwacht Anzahl" ]
                  , Html.option [ value "Koffein Konsum", selected ("Koffein Konsum" == model.droppdown1) ] [ Html.text "Koffein Konsum" ]       
                  , Html.option [ value "Alkohol Konsum", selected ("Alkohol Konsum" == model.droppdown1) ] [ Html.text "Alkohol Konsum" ]         
                  , Html.option [ value "Raucher", selected ("Raucher" == model.droppdown1) ] [ Html.text "Raucher: Bool" ]  
                  , Html.option [ value "Sport Einheiten", selected ("Sport Einheiten" == model.droppdown1) ] [ Html.text "Sport Einheiten" ]   ]                            
                  , Html.br [] []
                  , Html.br [] []
                  , Html.text "Min Value"
                  , Html.br [] []
                  ,input
                    [ type_ "text"
                    , placeholder "Bitte eingeben..."
         --           , Html.Attributes.value model.minFilter
                    , onInput MinFilterChanged
                  ]
                  []
                  , Html.text "Max Value"
                  , Html.br [] []
                  ,input
                    [ type_ "text"
                    , placeholder "Bitte eingeben..."
         --           , Html.Attributes.value model.minFilter
                    , onInput MaxFilterChanged
                  ]
                  []
   --             , button [ onClick SubmitInput ] [ text "Submit" ]
                  , Html.br [] []
                  , Html.br [] []
                  , Html.br [] []
                  , Html.u [ Html.Attributes.style "font-size" "16px" ] [Html.text "Blackbox"]
                  , Html.br [] []
                  , Html.br [] []
                  , Html.text "Y-Variable" 
                  , Html.br [] []
               , Html.select [ onInput Option2Selected ]
                  [ Html.option [ value "", selected ("" == model.droppdown2) ] [ Html.text "NA" ]
                  , Html.option [ value "Geschlecht", selected ("Geschlecht" == model.droppdown2) ] [ Html.text "Geschlecht Bool" ]
                  , Html.option [ value "Alter", selected ("Alter" == model.droppdown2) ] [ Html.text "Alter" ]
                  , Html.option [ value "Schlafdauer", selected ("Schlafdauer" == model.droppdown2) ] [ Html.text "Schlafdauer" ]
                  , Html.option [ value "Schlaf Effizienz", selected ("Schlaf Effizienz" == model.droppdown2) ] [ Html.text "Schlaf Effizienz" ]
                  , Html.option [ value "REM", selected ("REM" == model.droppdown2) ] [ Html.text "REM" ]
                  , Html.option [ value "Tiefschlaf Anteil", selected ("Tiefschlaf Anteil" == model.droppdown2) ] [ Html.text "Tiefschlaf Anteil" ]
                  , Html.option [ value "Leichtschlaf Anteil", selected ("Leichtschlaf Anteil" == model.droppdown2) ] [ Html.text "Leichtschlaf Anteil" ]  
                  , Html.option [ value "Erwacht Anzahl", selected ("Erwacht Anzahl" == model.droppdown2) ] [ Html.text "Erwacht Anzahl" ]
                  , Html.option [ value "Koffein Konsum", selected ("Koffein Konsum" == model.droppdown2) ] [ Html.text "Koffein Konsum" ]       
                  , Html.option [ value "Alkohol Konsum", selected ("Alkohol Konsum" == model.droppdown2) ] [ Html.text "Alkohol Konsum" ]         
                  , Html.option [ value "Raucher", selected ("Raucher" == model.droppdown2) ] [ Html.text "Raucher: Bool" ]  
                  , Html.option [ value "Sport Einheiten", selected ("Sport Einheiten" == model.droppdown2) ] [ Html.text "Sport Einheiten" ]
                 ]
                  , Html.br [] []
                  , Html.br [] []
                  , Html.br [] []
                  , Html.text "Z- Variable"
                  , Html.br [] []
               ,  Html.select [ onInput Option3Selected ]
                  [ Html.option [ value "", selected ("" == model.droppdown3) ] [ Html.text "NA" ]
                  , Html.option [ value "Geschlecht", selected ("Geschlecht" == model.droppdown3) ] [ Html.text "Geschlecht Bool" ]
                  , Html.option [ value "Alter", selected ("Alter" == model.droppdown3) ] [ Html.text "Alter" ]
                  , Html.option [ value "Schlafdauer", selected ("Schlafdauer" == model.droppdown3) ] [ Html.text "Schlafdauer" ]
                  , Html.option [ value "Schlaf Effizienz", selected ("Schlaf Effizienz" == model.droppdown3) ] [ Html.text "Schlaf Effizienz" ]
                  , Html.option [ value "REM", selected ("REM" == model.droppdown3) ] [ Html.text "REM" ]
                  , Html.option [ value "Tiefschlaf Anteil", selected ("Tiefschlaf Anteil" == model.droppdown3) ] [ Html.text "Tiefschlaf Anteil" ]
                  , Html.option [ value "Leichtschlaf Anteil", selected ("Leichtschlaf Anteil" == model.droppdown3) ] [ Html.text "Leichtschlaf Anteil" ]  
                  , Html.option [ value "Erwacht Anzahl", selected ("Erwacht Anzahl" == model.droppdown3) ] [ Html.text "Erwacht Anzahl" ]
                  , Html.option [ value "Koffein Konsum", selected ("Koffein Konsum" == model.droppdown3) ] [ Html.text "Koffein Konsum" ]       
                  , Html.option [ value "Alkohol Konsum", selected ("Alkohol Konsum" == model.droppdown3) ] [ Html.text "Alkohol Konsum" ]         
                  , Html.option [ value "Raucher", selected ("Raucher" == model.droppdown3) ] [ Html.text "Raucher: Bool" ]  
                  , Html.option [ value "Sport Einheiten", selected ("Sport Einheiten" == model.droppdown3) ] [ Html.text "Sport Einheiten" ]
                    ]                 
                  , Html.br [] []
                  , Html.br [] []
                  , Html.br [] []
    --              , Html.text 
                  , Html.br [] []
                ]]   
                ,  div []
                       
                    [header]
                                                    
               , let

                   daten : List (Aussortierte_Daten)
                   daten = 
                     case model.droppdown1 of
                      "Geschlecht" ->  model.daten |> List.filter (\d -> d.mygeschlecht >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.mygeschlecht <= Maybe.withDefault 10000 model.maxFilter)
                      "Alter" ->  model.daten |> List.filter (\d -> d.myalter >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myalter <= Maybe.withDefault 10000 model.maxFilter)
                      "Schlafdauer" -> model.daten |> List.filter (\d -> d.myschlafdauer >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myschlafdauer <= Maybe.withDefault 10000 model.maxFilter)        
                      "Schlaf Effizienz" -> model.daten |> List.filter (\d -> d.myschlaf_effizienz >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myschlaf_effizienz <= Maybe.withDefault 10000 model.maxFilter)         
                      "REM" -> model.daten |> List.filter (\d -> d.myrem_anteil >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myrem_anteil <= Maybe.withDefault 10000 model.maxFilter) 
                      "Tiefschlaf Anteil" -> model.daten |> List.filter (\d -> d.mytiefschlaf_anteil >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.mytiefschlaf_anteil <= Maybe.withDefault 10000 model.maxFilter)        
                      "Leichtschlaf Anteil" ->  model.daten |> List.filter (\d -> d.myleichtschlaf_anteil >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myleichtschlaf_anteil <= Maybe.withDefault 10000 model.maxFilter)         
                      "Erwacht Anzahl" -> model.daten |> List.filter (\d -> d.myerwacht_anzahl >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myerwacht_anzahl <= Maybe.withDefault 10000 model.maxFilter)  
                      "Koffein Konsum" ->  model.daten |> List.filter (\d -> d.mykoffein_konsum >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.mykoffein_konsum <= Maybe.withDefault 10000 model.maxFilter)         
                      "Alkohol Konsum" ->  model.daten |> List.filter (\d -> d.myalkohol_konsum >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myalkohol_konsum <= Maybe.withDefault 10000 model.maxFilter)
                      "Raucher" -> model.daten |> List.filter (\d -> d.myraucher >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.myraucher <= Maybe.withDefault 10000 model.maxFilter)
                      "Sport Einheiten" -> model.daten |> List.filter (\d -> d.mysport >= Maybe.withDefault 0 model.minFilter) |> List.filter (\d -> d.mysport <= Maybe.withDefault 10000 model.maxFilter)
                      _ -> []         
    

                   xList : List (Float)
                   xList =
                     case model.droppdown1 of
                      "Geschlecht" -> List.map .mygeschlecht daten
                      "Alter" -> List.map .myalter daten
                      "Schlafdauer" -> List.map .myschlafdauer daten
                      "Schlaf Effizienz" -> List.map  .myschlaf_effizienz daten
                      "REM" -> List.map .myrem_anteil daten
                      "Tiefschlaf Anteil" ->  List.map  .mytiefschlaf_anteil daten
                      "Leichtschlaf Anteil" -> List.map .myleichtschlaf_anteil daten
                      "Erwacht Anzahl" -> List.map .myerwacht_anzahl daten
                      "Koffein Konsum" -> List.map .mykoffein_konsum daten
                      "Alkohol Konsum" -> List.map .myalkohol_konsum daten
                      "Raucher" -> List.map .myraucher daten 
                      "Sport Einheiten" -> List.map .mysport daten
                      _ -> []    
     
                   yList : List (Float)
                   yList =
                     case model.droppdown2 of
                      "Geschlecht" -> List.map .mygeschlecht daten 
                      "Alter" -> List.map .myalter daten
                      "Schlafdauer" -> List.map .myschlafdauer daten
                      "Schlaf Effizienz" -> List.map  .myschlaf_effizienz daten
                      "REM" -> List.map .myrem_anteil daten
                      "Tiefschlaf Anteil" ->  List.map  .mytiefschlaf_anteil daten
                      "Leichtschlaf Anteil" -> List.map .myleichtschlaf_anteil daten
                      "Erwacht Anzahl" -> List.map .myerwacht_anzahl daten
                      "Koffein Konsum" -> List.map .mykoffein_konsum daten
                      "Alkohol Konsum" -> List.map .myalkohol_konsum daten
                      "Raucher" -> List.map .myraucher daten 
                      "Sport Einheiten" -> List.map .mysport daten
                      _ -> []      

                   zList : List (Float)
                   zList =
                     case model.droppdown3 of
                      "Geschlecht" -> List.map .mygeschlecht daten 
                      "Alter" -> List.map .myalter daten
                      "Schlafdauer" -> List.map .myschlafdauer daten
                      "Schlaf Effizienz" -> List.map  .myschlaf_effizienz daten
                      "REM" -> List.map .myrem_anteil daten
                      "Tiefschlaf Anteil" ->  List.map  .mytiefschlaf_anteil daten
                      "Leichtschlaf Anteil" -> List.map .myleichtschlaf_anteil daten
                      "Erwacht Anzahl" -> List.map .myerwacht_anzahl daten
                      "Koffein Konsum" -> List.map .mykoffein_konsum daten
                      "Alkohol Konsum" -> List.map .myalkohol_konsum daten
                      "Raucher" -> List.map .myraucher daten 
                      "Sport Einheiten" -> List.map .mysport daten
                      _ -> []    

-- Verhaltensindikatoren in einzelne Listen aufgebrochen. 
                   filteralter : List Float
                   filteralter =
                        List.map .myalter daten

                   filtergender : List Float
                   filtergender =
                        List.map .mygeschlecht daten
                     
                   filterkoffein : List(Float)
                   filterkoffein =
                      List.map .mykoffein_konsum daten
                    
                   filteralkohol: List(Float)
                   filteralkohol =
                      List.map .myalkohol_konsum daten

                   filterraucher: List(Float)
                   filterraucher =
                      List.map .myraucher daten 

                   filtersport: List(Float)
                   filtersport =
                      List.map .mysport daten

                   name =
                    List.map .myid_ daten

                   combinedListX_Scatter: List ScatterplottPoint 
                   combinedListX_Scatter =
                              (combineLists xList xList name) |> addNV

                   combinedListY_Scatter: List ScatterplottPoint 
                   combinedListY_Scatter =
                              (combineLists yList yList name) |> addNV

                   combinedListZ_Scatter: List ScatterplottPoint 
                   combinedListZ_Scatter =
                              (combineLists zList zList name) |> addNV

--Hier werden die Listen in für den Boxplott vorbereitet in diesem Fall istes wichtig, dass die XListe 
--Die Mittlere Liste ist, da wir ansonsten nicht das untersuchende Element in der Mitte haben. 
                   combinedList_Box : List MultiDimPoint
                   combinedList_Box = ParalleleKoordinatenRoengten.combineLists_Box name  yList xList zList 

                in
                   div [
                    Html.Attributes.style "margin-left" "15%" 
                    , Html.Attributes.style "padding" "2em"

                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "width" "80%"
                    , Html.Attributes.style "font-family" "Arial"] 
                    [div[ Html.Attributes.style "padding" "2em"
                         , Html.Attributes.style "margin-left" "15%" ]
                        [ Html.text " Scatterplot Norm QQ Plot: Hilft Ihnen zu analysieren ob ein Attribut normalverteilt ist. Wählen Sie dazu ein Attribut aus und geben Sie optional einen Min und Max Wert an. Die in Röngten ausgewählten Werte werden werden kleiner darunter angezeigt. "]
                        ,scatterplot 
                    { xDescription = "Normalverteilung"
                    , yDescription = model.droppdown1
                    , data = combinedListX_Scatter
                    }
                    , div [ Html.Attributes.style "height" "50%"
                         , Html.Attributes.style "width" "80%"
                         , Html.Attributes.style "display" "flex"
                         ,Html.Attributes.style "margin-left" "15%"
                         , Html.Attributes.style "padding" "2em" ]
                     [scatterplot 
                     { xDescription = "Normalverteilung"
                        , yDescription = model.droppdown2
                        , data = combinedListY_Scatter
                      }
                      ,scatterplot 
                       { xDescription = "Normalverteilung"
                         , yDescription = model.droppdown3
                         , data = combinedListZ_Scatter
                        }]
                          
                    ,div[ Html.Attributes.style "padding" "2em"
                         , Html.Attributes.style "margin-left" "15%" ]
                        [ Html.text " Röngten: Diese Visualisierung soll Ihnen dabei helfen Cluster und versteckte Ahängigkeiten zwischen den Attributen zu finden. "]
                          
                    ,(blackbox combinedList_Box)
                    ,div[ Html.Attributes.style "padding" "2em"
                         , Html.Attributes.style "margin-left" "15%" ]
                        [ Html.text " Hier sehen Sie die Korrelation der Verhaltensweisen und Ausprägungen als eine Art Scheeflocke angezeigt. Wenn Sie ein Attribut in Y und X augewählt wird dies Ihnen auch angezeigt. "]
                    ,graph {xdescriptor = {name = model.droppdown1, data = xList},
                            attribute = [{name = "Alter", data= filteralter},
                             {name= "Geschlecht", data= filtergender},
                             {name= "Koffein", data= filterkoffein},
                             {name= "Alkohol", data= filteralkohol},
                             {name= "Raucher", data= filterraucher},
                             {name= "Sport", data= filtersport}
                             ]
                             ++ (if model.droppdown2 /= "" then [{name = model.droppdown2, data = yList}] else [])
                             ++ if model.droppdown3 /= "" then [{name = model.droppdown3, data = zList}] else []
                             }

                   ,footer]
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
                (genderToFloat mygeschlecht )
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
                0.0
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
                         myrem_anteil = myXY* 0.1 * myXB * myXA,
                         mytiefschlaf_anteil = myXV *0.1* myXB * myXA,
                         myleichtschlaf_anteil = myXK* 0.1 * myXB * myXA
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
                         myraucher = raucherToFloat myXV,
                         mysport = myXK
                          }
                    )
                    c.erwacht_anzahl
                    c.koffein_konsum
                    c.alkohol_konsum
                    c.raucher
                    c.sport
            )

-- Hilfsfunktion, die "Yes" zu 2 und "No" zu 1 zuordnet Bewusst nicht 0 und 1 genommen, da wir so eine bessere Darstellung im Scatterplot haben.
raucherToFloat : String -> Float
raucherToFloat value =
    case value of
        "Yes" -> 2
        "No" -> 1
        _ -> 0

genderToFloat : String -> Float
genderToFloat value =
    case value of
        "Male" -> 1
        "Female" -> 2
        _ -> 0

type alias Aussortierte_Daten =
 {myid_ :  String 
 ,myalter : Float
 ,mygeschlecht : Float
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
 ,myraucher : Float
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

 
--Dieser Bereich ist für das Vorbereiten der Daten für den Scatterplott.--Variablen zur Beeinflussung der Dartstellungen

