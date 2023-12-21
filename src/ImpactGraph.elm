module ImpactGraph exposing (..)

import Scatterplot exposing (w, h, padding, radius, tickCount)



{-Grundsätzlicher Plan:

Ziel ist es die korrelation des gewählten Attributes auf die Variable x zu berechnen. 
Hier zu wollen wir ausgewählte Einflussfaktoren nehmen und auf den ausgewählten Datensatz x berechnen sowie auf die gewählten Werte y und z.

Die Korrelation wird repräsentiert durch r 
0 bis 1 = Positive korrelation
-1 bis o = negative korrelation 

Nähe der Werte zu 0 = keine Korrelation
Im Zentrum ist das gewählte Attribut x oder y oder z
un drum herum sind die Einflussfaktoren die nähe gibt die Korrelation an
Und die Farbe die stärke der Korrelation 

-}
--Gibt die Summe der Liste zurück
summe : List Float -> Float
summe list = 
    List.sum list