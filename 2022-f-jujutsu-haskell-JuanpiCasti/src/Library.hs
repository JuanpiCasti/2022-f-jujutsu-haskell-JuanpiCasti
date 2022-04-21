module Library where
import PdePreludat

doble :: Number -> Number
doble n = n * 2

-----------------------
-- Definiciones de tipos
data Hechicero = Hechicero {
    nombre :: String,
    antiguedad :: Number,
    clan :: String,
    grado :: Number
    } deriving (Eq, Ord, Show)

type Grupo = [Hechicero]

-- Experiencia
minimoExperiencia :: Number
minimoExperiencia = 1
tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechicero = antiguedad hechicero > minimoExperiencia

soloExpertos :: Grupo -> Grupo
soloExpertos grupo = filter tieneExperiencia grupo

-- Preparacion
minimoPreparado :: Number
minimoPreparado = 3
estanPreparados :: Grupo -> Bool
estanPreparados grupo = (length grupo) > minimoPreparado

-- Relacionado a grado
gradoEspecial :: Number
gradoEspecial = 0
esGradoEspecial :: Hechicero -> Bool
esGradoEspecial hechicero = (grado hechicero) == gradoEspecial
subirDeGrado :: Hechicero -> Hechicero
subirDeGrado hechicero | esGradoEspecial hechicero = hechicero
                       | otherwise = (Hechicero (nombre hechicero) (antiguedad hechicero) (clan hechicero) ((grado hechicero) - 1))

sonInvencibles :: Grupo -> Bool
sonInvencibles grupo = any esGradoEspecial grupo

powerUp :: Grupo -> Grupo
powerUp grupo = map subirDeGrado grupo

soloEspeciales :: Grupo -> Grupo
soloEspeciales grupo = filter esGradoEspecial grupo

cantidadEspeciales :: Grupo -> Number
cantidadEspeciales grupo = length (soloEspeciales grupo)
-- Relacionado a clanes
clanesPrestigiosos :: [String]
clanesPrestigiosos = ["Zening", "Gojo", "Kamo"]
esPrestigioso :: Hechicero -> Bool
esPrestigioso hechicero = elem (clan hechicero) clanesPrestigiosos

sonFavoritos :: Grupo -> Bool
sonFavoritos grupo = all esPrestigioso grupo


-- 9
haceFrente :: Grupo -> Bool
haceFrente grupo = sonInvencibles grupo || estanPreparados grupo

-- Data de prueba
pepito = Hechicero "Pepito" 1 "Pepe" 0
pepazo = Hechicero "Pepazo" 1 "Gojo" 4
jorgito = Hechicero "Jorgito" 2 "Zening" 2
juancito = Hechicero "Juancito" 3 "Kamo" 5
grupo1 = [pepito, pepazo, jorgito, juancito]
grupo2 = [jorgito, pepazo]