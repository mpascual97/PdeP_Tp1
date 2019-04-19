data Auto = Auto{
nombre :: String,
nivelNafta :: Float,
velocidad :: Int,
enamorade :: String,
truco :: String
} deriving (Show)


deReversa :: Auto -> Auto
deReversa unAuto = unAuto {nivelNafta = ((+200).nivelNafta) unAuto}

impresionar :: Auto -> Auto
impresionar unAuto = unAuto {velocidad = ((*2).velocidad) unAuto}

nitro :: Auto -> Auto
nitro unAuto = unAuto {velocidad = ((+15).velocidad) unAuto}

fingirAmor :: Auto -> String -> Auto
fingirAmor unAuto falsoEnamorade = unAuto {enamorade = falsoEnamorade}

-- PUNTO 2
esVocal letra = letra == 'a' || letra == 'e' || letra == 'i' ||letra == 'o' ||letra == 'u'  

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad unAuto
 | (length.filter (esVocal).enamorade) unAuto < 3 = unAuto {velocidad = ((+15).velocidad) unAuto}
 | (length.filter (esVocal).enamorade) unAuto < 5 = unAuto {velocidad = ((+20).velocidad) unAuto}
 | otherwise = unAuto {velocidad =((+30).velocidad) unAuto}