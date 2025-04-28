module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data poderes = unPoder {
    nombre :: String
    danio :: Num
    curacion :: Num
    parametroExtra :: Int
}

data personaje = unPersonaje {
    nombre :: String
    poderBasico :: unPoder
    superPoder :: unPoder
    superPoderActivo :: Bool
    vida :: Int
}


unPoder bolaEspinosa = unPoder "Bola Espinosa" 1000 0 0
unPoder lluviaDeTuercas = unPoder "Lluvia de tuercas" 0 800 0
unPoder granadaDeEspina = unPoder "Granada de espinas" 0 0 5 
unPoder torretaCurativa = unPoder "Torreta curativa" 0 0 0

unPersonaje pamela = unPersonaje "Pamela" lluviaDeTuercas torretaCurativaFalse 9600
unPersonaje espina = unPersonaje "Espina" bolaEspinosa granadaDeEspina True 4800

atacarPoderEspecial unPersonaje personajeColega personajeContricante
    |unPersonaje.superPoderActivo = realizarSuperAtaque unPersonaje personajeColega personajeContricante
    |otherwise = "Super Poder desactivado"

realizarSuperAtaque unPersonaje personajeColega personajeContricante
    |unPersonaje.superPoder.nombre == "Granada de espinas" = atacarGranadaEspina unPersonaje.superPoder personajeContricante && atacarBolaEspinosa
    |unPersonaje.superPoder.nombre == "Torreta curativa"   = atacarTorretaCurativa unPersonaje personajeColega && atacarLluviaDeTuercas
    |otherwise = "Error"

atacarGranadaEspina :: unPoder->unPersonaje->unPersonaje
atacarGranadaEspina unPoder personajeContricante
    |unPoder.parametroExtra > 3 && personajeContricante.vida < 800 = personajeContricante.nombre = "Espina estuvo aqui" && personajeContricante.vida = 0
    |unPoder.parametroExtra > 3 = personajeContricante.nombre = "Espina estuvo aqui"
    |unPoder.parametroExtra =< 3 = atacarBolaEspinosa personajeContricante

atacarLluviaDeTuercas

atacarTorretaCurativa

atacarBolaEspinosa :: unPersonaje -> unPersonaje
atacarBolaEspinosa personajeContricante = personajeContricante.vida (max 0 (personajeContricante.vida - 1000)) /* Sin terminar*/
