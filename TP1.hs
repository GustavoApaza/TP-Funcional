-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
-- Dominio
-- Una persona tiene 
-- un nombre,
-- su edad que se mide en años,
-- un nivel de stress que va de 0 a 100, 
-- una lista de sus preferencias, que indican si un destino le gusta (ver más adelante)
-- y sus amigues

-- Punto 1: Datos de una persona
-- Puntos de scoring (integrante 1)
-- Queremos saber los puntos de scoring de una persona, que se da de la siguiente manera:
-- si tiene una cantidad par de amigues, multiplica el nivel de stress por la edad
-- si en cambio tiene más de 40 años, multiplica la cantidad de amigues por la edad
-- de lo contrario, es la cantidad de letras del nombre * 2
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
type Nombre = String
type Edad = Int
type NivelDeStress = Int
type ListaDePreferenciasDeDestinos = [String]
type ListaDeAmigos = [String]

type Persona = (Nombre, Edad, NivelDeStress, ListaDePreferenciasDeDestinos, ListaDeAmigos)
persona :: Persona
--persona = ("Juan", 25, 10, ["Las Vegas", "Japon", "Egipto"], ["Ana", "Gonzalo"])
--persona = ("Juan", 41, 10, ["Las Vegas", "Japon", "Egipto"], ["Ana"])
persona = ("Rigoberta", 31, 10, ["Las Vegas", "Japon", "Egipto"], ["Ana"])

nombre (nombre, _, _, _, _) = nombre
edad (_, edad, _, _, _) = edad
nivelDeStress (_, _, nivelDeStress, _, _) = nivelDeStress
preferenciasDeDestino (_, _, _, preferenciasDeDestino, _) = preferenciasDeDestino
amigos (_, _, _, _, amigos) = amigos

obtenerNombre :: Persona -> String
obtenerNombre persona = nombre persona

obtenerEdad :: Persona -> Int
obtenerEdad persona = edad persona

obtenerNivelDeStress :: Persona -> Int
obtenerNivelDeStress persona = nivelDeStress persona

obtenerPreferenciasDeDestino :: Persona -> [String]
obtenerPreferenciasDeDestino persona = preferenciasDeDestino persona

obtenerAmigos :: Persona -> [String]
obtenerAmigos persona = amigos persona

scoringDePersona persona 
    | (even.length.amigos) persona = (nivelDeStress persona) * (edad persona)
    | edad persona > 40 = length (amigos persona) * (edad persona)
    | otherwise = (length (nombre persona)) * 2