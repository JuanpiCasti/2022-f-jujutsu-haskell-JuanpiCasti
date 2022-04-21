module Library where
import PdePreludat

doble :: Number -> Number
doble n = n * 2

-----------------------
-- 1
data Hechicero = Hechicero {
    nombre :: String,
    antiguedad :: Number,
    clan :: String,
    grado :: Number
    } deriving (Eq, Ord, Show)

type Grupo = [Hechicero]

-- 2
minimoExperiencia :: Number
minimoExperiencia = 1
tieneExperiencia :: Hechicero -> Bool
tieneExperiencia hechicero = antiguedad hechicero > minimoExperiencia


-- 3
minimoPreparado :: Number
minimoPreparado = 3
estanPreparados :: Grupo -> Bool
estanPreparados grupo = (length grupo) > minimoPreparado

-- 4
gradoEspecial :: Number
gradoEspecial = 0
esGradoEspecial :: Hechicero -> Bool
esGradoEspecial hechicero = (grado hechicero) == gradoEspecial
subirDeGrado :: Hechicero -> Hechicero
subirDeGrado hechicero | esGradoEspecial hechicero = hechicero
                       | otherwise = (Hechicero (nombre hechicero) (antiguedad hechicero) (clan hechicero) ((grado hechicero) - 1))

-- 5
clanesPrestigiosos :: [String]
clanesPrestigiosos = ["Zening", "Gojo", "Kamo"]
esPrestigioso :: Hechicero -> Bool
esPrestigioso hechicero = elem (clan hechicero) clanesPrestigiosos

-- 6
sonInvencibles :: Grupo -> Bool
sonInvencibles grupo = any esGradoEspecial grupo

-- 7
sonFavoritos :: Grupo -> Bool
sonFavoritos grupo = all esPrestigioso grupo

-- 8
soloExpertos :: Grupo -> Grupo
soloExpertos grupo = filter tieneExperiencia grupo

-- 9 a
haceFrente :: Grupo -> Bool
haceFrente grupo = sonInvencibles grupo || estanPreparados grupo

-- 9 b
powerUp :: Grupo -> Grupo
powerUp grupo = map subirDeGrado grupo


-- 10
soloEspeciales :: Grupo -> Grupo
soloEspeciales grupo = filter esGradoEspecial grupo

cantidadEspeciales :: Grupo -> Number
cantidadEspeciales grupo = length (soloEspeciales grupo)

-- 11
promedioGrados :: Grupo -> Number
promedioGrados grupo = sum(map grado grupo) / length grupo


-- Data de prueba
pepito = Hechicero "Pepito" 1 "Pepe" 0
pepazo = Hechicero "Pepazo" 1 "Gojo" 4
jorgito = Hechicero "Jorgito" 2 "Zening" 2
juancito = Hechicero "Juancito" 3 "Kamo" 5
grupo1 = [pepito, pepazo, jorgito, juancito]
grupo2 = [jorgito, pepazo]