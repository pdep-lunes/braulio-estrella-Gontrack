module Lib () where

data Poder = Poder {
    nombrePoder :: String,
    danio :: Int,
    curacion :: Int,
    parametroExtra :: Int
}

data Personaje = Personaje {
    nombre :: String,
    poderBasico :: Poder,
    superPoder :: Poder,
    superPoderActivo :: Bool,
    vida :: Int
}


bolaEspinosa :: Poder
bolaEspinosa = Poder "Bola Espinosa" 1000 0 0

lluviaDeTuercas :: Poder
lluviaDeTuercas = Poder "Lluvia de tuercas" 0 800 0

tipoLluviaDeTuercas :: String
tipoLluviaDeTuercas = "sanadoras" --"daninas"

granadaDeEspina :: Poder
granadaDeEspina = Poder "Granada de espinas" 0 0 5

torretaCurativa :: Poder
torretaCurativa = Poder "Torreta curativa" 0 0 0

pamela :: Personaje
pamela = Personaje "Pamela" lluviaDeTuercas torretaCurativa False 9600

espina :: Personaje
espina = Personaje "Espina" bolaEspinosa granadaDeEspina True 4800


atacarConElpoderEspecial :: Personaje -> Personaje -> Personaje -> (String,Personaje,Personaje,Personaje)
atacarConElpoderEspecial usuario aliado enemigo
    |not (superPoderActivo usuario) = ("Super poder desactivado",usuario,aliado,enemigo)
    |nombrePoder (superPoder usuario) == "Granada de espinas" = atacarGranadaEspina usuario aliado enemigo
    |nombrePoder (superPoder usuario) == "Torreta curativa" = atacarTorretaCurativa usuario aliado enemigo
    |otherwise = ("Poder desconocido",usuario,aliado,enemigo)

atacarGranadaEspina :: Personaje -> Personaje -> Personaje -> (String,Personaje,Personaje,Personaje)
atacarGranadaEspina usuario aliado enemigo
    |parametroExtra (superPoder usuario) > 3 && vida enemigo < 800 =
        ("Granada de espina activada",usuario,aliado,
        Personaje
        (nombre enemigo ++ "Espina estuvo aqui")
        (poderBasico enemigo)
        (superPoder enemigo)
        False
        0)
    |parametroExtra (superPoder usuario) > 3 =
        ("Granada de espina y Bola espinosa activada",usuario,aliado,
        Personaje
        (nombre enemigo ++ "Espina estuvo aqui")
        (poderBasico enemigo)
        (superPoder enemigo)
        (superPoderActivo enemigo)
        (max 0 $ vida enemigo-danio (poderBasico usuario)))
    |otherwise = ("Granada de espina y Bola espinosa activada",usuario,aliado,
        Personaje
        (nombre enemigo)
        (poderBasico enemigo)
        (superPoder enemigo)
        (superPoderActivo enemigo)
        (max 0 $ vida enemigo-danio (poderBasico usuario)))

atacarTorretaCurativa :: Personaje -> Personaje -> Personaje -> (String,Personaje,Personaje,Personaje)
atacarTorretaCurativa usuario aliado enemigo
    |tipoLluviaDeTuercas == "sanadoras" =
        ("Torre Curativa y Lluvia de tuercas activado",usuario,
        Personaje
        (nombre aliado)
        (poderBasico aliado)
        (superPoder aliado)
        True
        (vida aliado*2 + curacion (poderBasico usuario))
        ,enemigo)

    |tipoLluviaDeTuercas == "daninas" =
        ("Torre Curativa y Lluvia de tuercas activado",usuario,
        Personaje
        (nombre aliado)
        (poderBasico aliado)
        (superPoder aliado)
        True
        (vida aliado*2 + curacion (poderBasico usuario))
        ,
        Personaje
        (nombre enemigo)
        (poderBasico enemigo)
        (superPoder enemigo)
        (superPoderActivo enemigo)
        (div (vida enemigo) 2)
        )
    |otherwise = ("Torre Curativa",usuario,
        Personaje
        (nombre aliado)
        (poderBasico aliado)
        (superPoder aliado)
        True
        (vida aliado*2)
        ,enemigo)

brawlers :: [Personaje]
brawlers = [pamela,espina]

quienesEstanEnLasUltimas :: [Personaje] -> [String]
quienesEstanEnLasUltimas unosbrawlers = map nombre (filter brawlersConPocaVida unosbrawlers)

brawlersConPocaVida :: Personaje -> Bool
brawlersConPocaVida unPersonaje = vida unPersonaje <= 800

