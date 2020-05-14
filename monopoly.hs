import Data.List 
import Text.Show.Functions 

type Propiedad = (String,Int) 
type Accion = (Jugador->Jugador)
data Jugador = Jugador {
nombre :: String,
dinero :: Int,
tacticaEnElJuego :: String, 
propiedadesActuales :: [Propiedad],
acciones :: [Accion]
}deriving (Show)

carolina :: Jugador
carolina = Jugador "Carolina" 500 "Accionista" [] [pasarPorElBanco,pagarAAccionistas]

manuel :: Jugador
manuel = Jugador "Manuel" 500 "Oferente Singular" [] [enojarse,pasarPorElBanco]

esAccionista :: Jugador->Bool
esAccionista (Jugador _ _ laTacticaEnElJuego _ _) = laTacticaEnElJuego == "Accionista"

esOferente :: Jugador->Bool
esOferente  (Jugador _ _ laTacticaEnElJuego _ _) = laTacticaEnElJuego == "Oferente Singular"

agregarDinero :: Int->Accion
agregarDinero dineroAAgregar jugador = jugador {dinero = dinero jugador + dineroAAgregar}

agregarPropiedad :: Propiedad->Accion
agregarPropiedad propiedadActual jugador = jugador {propiedadesActuales = propiedadesActuales jugador ++ [propiedadActual]}

pasarPorElBanco :: Accion
pasarPorElBanco jugador = agregarDinero 50 (jugador {tacticaEnElJuego = "Comprador compulsivo"})

enojarse :: Accion
enojarse unJugador = agregarDinero 40 (unJugador {acciones = acciones unJugador ++ [gritar]})

gritar :: Accion
gritar elJugador = elJugador {nombre = "AHHHH " ++ nombre elJugador}

subastar :: Propiedad -> Accion
subastar propiedad jugador  | (esAccionista jugador) || (esOferente jugador) = 
   agregarDinero (- (snd propiedad)) (agregarPropiedad propiedad jugador)
                           | otherwise = jugador

alquiler :: Int -> Int
alquiler unValor | unValor < 150 = 10
                 | otherwise = 20

valorDePropiedades :: Jugador -> [Int]
valorDePropiedades (Jugador _ _ _ lasPropiedadesActuales _) = map snd lasPropiedadesActuales

cobrarAlquileres :: Accion
cobrarAlquileres algunJugador = agregarDinero (sum (map alquiler (valorDePropiedades algunJugador))) algunJugador

pagarAAccionistas :: Accion
pagarAAccionistas jugador | esAccionista jugador = agregarDinero 200 jugador
                          | otherwise = agregarDinero (-100) jugador

puedeComprar :: Propiedad->Jugador->Bool
puedeComprar laPropiedad (Jugador _ dineroActual _ _ _) = (snd laPropiedad) <= dineroActual

hacerBerrinchePor :: Propiedad->Accion
hacerBerrinchePor unaPropiedad jugador | puedeComprar unaPropiedad jugador = 
   agregarDinero (-(snd unaPropiedad)) (agregarPropiedad unaPropiedad jugador)
                                       | otherwise = hacerBerrinchePor unaPropiedad (gritar (agregarDinero 10 jugador)) 

listaDeAcciones :: Jugador -> [Accion]
listaDeAcciones (Jugador _ _ _ _ lasAcciones) = lasAcciones

ejecutarLasAcciones :: [Accion]->Accion
ejecutarLasAcciones unasAcciones = foldl (.) id unasAcciones

ultimaRonda :: Accion
ultimaRonda jugador = (ejecutarLasAcciones (listaDeAcciones jugador)) jugador

dineroActualDelJugador :: Jugador -> Int
dineroActualDelJugador (Jugador _ elDinero _ _ _) = elDinero

compararDineroDeJugadores :: Jugador->Accion
compararDineroDeJugadores jugador otroJugador 
   | (dineroActualDelJugador jugador) < (dineroActualDelJugador otroJugador) = otroJugador
   | otherwise =  jugador

juegoFinal :: Jugador->Accion
juegoFinal jugador otroJugador = compararDineroDeJugadores (ultimaRonda jugador) (ultimaRonda otroJugador)
