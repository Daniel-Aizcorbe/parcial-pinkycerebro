import Data.List

data Animal = UnAnimal {
    especie :: String,
    coeficienteIntelectual :: Int,
    capacidades :: [String]
} deriving (Show)

jorge :: Animal
jorge = UnAnimal{
    especie = "Caballo",
    coeficienteIntelectual = 130,
    capacidades = ["hablar","programar en Haskell"]
}

jake :: Animal
jake = UnAnimal {
    especie = "Perro",
    coeficienteIntelectual = 50,
    capacidades = ["Transformar su cuerpo"]
}

type Transformacion = Animal -> Animal

inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior cantidad animal = animal {coeficienteIntelectual = (coeficienteIntelectual animal) + cantidad}

pinkificar :: Transformacion
pinkificar animal = animal {capacidades = []}

superpoderes :: Transformacion
superpoderes animal
    | especie animal == "Elefante"                                    = agregarCapacidad "no tenerle miedo a los ratones" animal
    | especie animal == "Raton" && coeficienteIntelectual animal >100 = agregarCapacidad  "hablar" animal
    | otherwise                                                       = animal     

agregarCapacidad :: String -> Animal -> Animal
agregarCapacidad capacidad animal = animal {capacidades = capacidad:(capacidades animal)}

type Criterio = Animal -> Bool

antropomorfico :: Criterio
antropomorfico animal = tieneLaCapacidad "hablar" animal && coeficienteIntelectual animal >100

tieneLaCapacidad :: String -> Animal -> Bool
tieneLaCapacidad capacidad animal = elem capacidad $ capacidades animal

noTanCuerdo :: Criterio
noTanCuerdo animal = (>2).length.filter (pinkiesco) $ capacidades animal

pinkiesco :: String -> Bool
pinkiesco capacidad = ((=="hacer").take 5 $ capacidad) && ((esPinkieska).drop 6 $ capacidad)

esPinkieska :: String -> Bool
esPinkieska capacidad = ((>0).length.filter (esVocal) $ capacidad) && ((<5).length $ capacidad)

esVocal :: Char -> Bool
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False

type Experimento = (Criterio,[Transformacion])

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso (criterio,listaDeTransformaciones) = criterio.realizarExperimento listaDeTransformaciones

realizarExperimento :: [Transformacion] -> Animal -> Animal
realizarExperimento listaDeTransformaciones animal = foldr ($) animal $ reverse listaDeTransformaciones

-- EJEMPLO DE CONSULTA

ratonDeEjemplo :: Animal
ratonDeEjemplo = UnAnimal {
    especie = "Raton",
    coeficienteIntelectual = 17,
    capacidades = ["destrenglonir el mundo","hacer planes desalmados"]
} 

experimentoDeEjemplo :: Experimento
experimentoDeEjemplo = (antropomorfico,[pinkificar,inteligenciaSuperior 10,superpoderes])

ejemplo :: Bool
ejemplo = experimentoExitoso experimentoDeEjemplo ratonDeEjemplo

reporteDeCoeficientesIntelectualesDeAnimalesConAlgunaCapacidad :: [Animal] -> [String] -> Experimento -> [Int]
reporteDeCoeficientesIntelectualesDeAnimalesConAlgunaCapacidad animales listaDeCapacidades experimento = map (coeficienteIntelectual) $ animalesQueCumplenRequisitosDelReporte (tieneAlgunaDe) animales listaDeCapacidades experimento

tieneAlgunaDe :: [String] -> Animal -> Bool
tieneAlgunaDe listaDeCapacidades animal = (/=[]).intersect listaDeCapacidades $ capacidades animal

reporteAnimalesConTodasLasCapacidades :: [Animal] -> [String] -> Experimento -> [Animal]
reporteAnimalesConTodasLasCapacidades = animalesQueCumplenRequisitosDelReporte (tieneTodas)

tieneTodas :: [String] -> Animal -> Bool
tieneTodas listaDeCapacidades animal = listaDeCapacidades == capacidades animal

reporteAnimalesConNingunaCapacidad :: [Animal] -> [String] -> Experimento -> [Animal]
reporteAnimalesConNingunaCapacidad  = animalesQueCumplenRequisitosDelReporte (noTieneNinguna)
    
noTieneNinguna :: [String] -> Animal -> Bool
noTieneNinguna listaDeCapacidades = not.tieneTodas listaDeCapacidades 

animalesQueCumplenRequisitosDelReporte :: ([String] -> Animal -> Bool) -> [Animal] -> [String] -> Experimento -> [Animal]
animalesQueCumplenRequisitosDelReporte cuantasCapacidadesDebeTener animales listaDeCapacidades experimento = filter (cuantasCapacidadesDebeTener listaDeCapacidades) $ map (realizarExperimento (snd experimento)) animales

{-
EXPERIMENTOS QUE SE PODRIAN REALIZAR EN UN ANIMAL CON INFINITAS CAPACIDADES
-}
experimento1 = (antropomorfico,[pinkificar,inteligenciaSuperior 10])
experimento2 = (noTanCuerdo,[superpoderes,inteligenciaSuperior 19,pinkificar])

-- depende como se entienda lo de "infinitas habilidades"

--todasLasPalabrasPinkirescas = filter esPinkieska $ generateWordsUpTo 4