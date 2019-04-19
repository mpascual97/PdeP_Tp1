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
 
 -- PUNTO 3
 
puedeUsarTruco :: Auto -> Bool
puedeUsarTruco unAuto = (nivelNafta unAuto) > 0 && (velocidad  unAuto< 100)
-- seguro se puede mejorar con aplicación parcial

-- PUNTO 4
comboLoco :: Auto -> Auto
comboLoco = deReversa.nitro

queTrucazo :: Auto -> String -> Auto
queTrucazo unAuto falsoEnamorade = incrementarVelocidad (fingirAmor unAuto falsoEnamorade)

turbo :: Auto -> Auto
turbo unAuto = unAuto {nivelNafta = 0, velocidad = velocidad unAuto + round((nivelNafta unAuto) *10)} 