-------------------------------------------------------------------------------
-- Autores: 
-- 	Pablo Reina Jimenez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-- 	Maria Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es 
-------------------------------------------------------------------------------

module CentrosAleatorios 
    (generaCentros
    ) where 

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo tiene por objeto generar los centros aleatorios que inicializan el
-- algoritmo de k-means
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados
import Data.Array
import Data.List
import System.Random
import Distancias 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Lista de funciones del modulo

-- Funcion listaMinMax :: [Vector] -> [(Double, Double)]
-- Recibe por parametros una lista de vectores (correspondiente al dataset)
-- y retorna una lista de (maximo, minimo) de cada coordenada a partir de los
-- vectores del dataset. 
-- El objetivo es utilizarla para generar los centros aleatorios teniendo en cuenta
-- en que rango se mueven los valores de cada coordenada.

-- Parametros:     
-- lvectores :: [Vector]                    Lista de vectores del dataset
-- Resultado:
-- [(min, max)] :: [(Double, Double)]       Lista de valores maximo y minimo de cada coordenada

-- Funciones relacionadas:
-- minMaxCoord :: Int -> [Vector] -> (Double, Double)   -- Dada una coordenada y los datos devuelve  
                                                        -- los valores (minimo, maximo) de la misma
-- extraeValoresCoord :: Int -> [Vector] -> [Double]    -- Dada una coordenada y los datos devuelve 
                                                        -- la lista de valores que toma esa coordenada

listaMinMax :: [Vector] -> [(Double, Double)]
listaMinMax lvectores
    | null lvectores = error "No se pueden inicializar los centros a partir de un dataset vacio"
    | otherwise = foldr(\coord rr -> (minMaxCoord coord lvectores) : rr) [] lcoord
        where   dimension = (snd.bounds.head) lvectores
                lcoord = [1..dimension]

minMaxCoord :: Int -> [Vector] -> (Double, Double)
minMaxCoord coord lvectores = (minimum lcoord, maximum lcoord)
    where lcoord = extraeValoresCoord coord lvectores

extraeValoresCoord :: Int -> [Vector] -> [Double]
extraeValoresCoord coord lvectores = map(\v -> v!coord) lvectores


-- Funcion generaCentros :: Int -> [Vector] -> IO [Vector]
-- Recibe por parametros el numero de centros y lista de vectores (correspondiente 
-- al dataset) y retorna una lista de centros aleatorios que inicializan el algoritmo
-- de k-medias.

-- Parametros:     
-- k :: Int                                 Numero de centros iniciales
-- lvectores :: [Vector]                    Lista de vectores del dataSet
-- Resultado:
-- [centros] :: IO [Vector]                 Lista de centros iniciales

-- Funciones relacionadas:
-- listaVectorIO :: IO [Double] -> IO Vector                    -- Transforma una lista de valores en un vector  
-- generaUnNumero :: Int -> [(Double,Double)] -> IO [Double]    -- Dada una lista de intervalos para cada coordenada
                                                                -- genera un vector donde cada coordenada se mueva 
                                                                -- dentro del intervalo correspondiente
                                                                -- Genera los vectores como listas de valores                                                               -- 

generaCentros :: Int -> [Vector] -> IO [Vector]
generaCentros k lvectores
    | k == 0 = return []
    | otherwise = do 
        let listaMinMax2 = listaMinMax lvectores -- [(0,0.5),(0,0.5)]
        x <- listaVectorIO (generaUnNumero 0 listaMinMax2)
        ys <- (generaCentros (k-1) lvectores)
        return ([x] ++ ys)

listaVectorIO :: IO [Double] -> IO Vector
listaVectorIO xs = do
    xs' <- xs
    return  (array (1,length xs') [(y,x) | (x,y) <- (zip xs' [1..length xs'])])

generaUnNumero :: Int -> [(Double,Double)] -> IO [Double]
generaUnNumero i xs
    | i == (length xs) = return [] 
    | otherwise =  do
        gen <- newStdGen
        let (randNumber, newGen) = randomR (xs!!i) gen :: (Double, StdGen)
        ys <- (generaUnNumero (i+1) xs)
        return (randNumber:ys)
