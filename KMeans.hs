-------------------------------------------------------------------------------
-- Autores: 
-- 	Pablo Reina Jimenez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-- 	Maria Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es 
-------------------------------------------------------------------------------

module KMeans 
    ( kMeans,
      kMeansCompleto,
      asocXM
    ) where 

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo tiene por objeto implementar el algoritmo de k-means. 

-- El algoritmo comienza con k centros aleatorios y cada punto se asocia al centro
-- mas cercano. Cada centro se vuelve a calcular en funcion a los puntos que tiene
-- asociado. El proceso se detiene cuando los centros no cambian de una iteracion a otra
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados
import Distancias
import Data.Array
import Data.List
import System.Random
import CentrosAleatorios
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Lista de funciones del modulo

-- Funcion kMeans :: Int -> [Vector] -> Distancia -> IO [Vector]
-- Recibe por parametros un entero (numero de centros), una lista de vectores (correspondiente al dataset) 
-- y una funcion distancia y retorna una lista de vectores (centros finales)
-- El objetivo es utilizarla para generar los centros finales del algoritmo

-- Parametros:     
-- k :: Int                   Numero de centros
-- xs :: [Vector]             Lista de vectores del dataset
-- d :: Distancia             Funcion distancia
-- Resultado:
-- [centros] :: [Vector]      Lista de centros finales

kMeans :: Int -> [Vector] -> Distancia -> IO [Vector]
kMeans k xs d = do
    m <- generaCentros k xs
    return (kMeansAux xs m d)


-- Funcion kMeansCompleto :: Int -> [Vector] -> Distancia -> IO [(Vector, Vector)]
-- Recibe por parametros un entero (numero de centros), una lista de vectores (correspondiente al dataset) 
-- y una funcion distancia y retorna una lista de tuplas de vectores (vectores del dataset asociados a un centro)
-- El objetivo es utilizarla para generar la asociacion de vectores iniciales y centros finales

-- Parametros:     
-- k :: Int                   Numero de centros
-- xs :: [Vector]             Lista de vectores del dataset
-- d :: Distancia             Funcion distancia
-- Resultado:
-- [(vector, centro)] :: IO [(Vector, Vector)]      Lista que asocia a cada vector inicial su centro de cluster más cercano

kMeansCompleto :: Int -> [Vector] -> Distancia -> IO [(Vector, Vector)]
kMeansCompleto k xs d = do
    m <- generaCentros k xs 
    let mFinal = kMeansAux xs m d
    return (asocXM xs mFinal d)


-- Funcion kMeansAux :: [Vector] -> [Vector] -> Distancia -> [Vector]
-- Recibe por parametros una lista de vectores (correspondiente al dataset), 
-- una lista de vectores (correspondiente a los centros actuales) 
-- y una funcion distancia y retorna una lista de vectores (centros actuales)
-- El objetivo es ir iterando esta funcion hasta que los centros actuales
-- coincidan con los de la última iteracion

-- Parametros:
-- xs :: [Vector]             Lista de vectores del dataset
-- m :: [Vector]              Lista de centros de la ultima iteracion
-- d :: Distancia             Funcion distancia
-- Resultado:
-- [centros] :: [Vector]      Lista de centros actualizada

kMeansAux :: [Vector] -> [Vector] -> Distancia -> [Vector]
kMeansAux xs m d
    | getNewM xms m == m = m
    | otherwise = kMeansAux xs (getNewM xms m) d
        where xms = asocXM xs m d


-- Funcion getNewM :: [(Vector, Vector)] -> [Vector] -> [Vector]
-- Recibe por parametros una lista de tuplas que asocia a cada vector
-- del dataset un centro, una lista con los centros actuales 
-- y una funcion distancia y retorna una lista de vectores (centros actualizados)
-- El objetivo es calcular la nueva lista de centros mediante la media de los
-- puntos asociados a cada centro

-- Parametros:
-- xms :: [(Vector, Vector)]   Lista de tuplas que asocia a cada vector del dataset su centro mas cercano
-- ms :: [Vector]              Lista de centros actuales
-- d :: Distancia              Funcion distancia
-- Resultado:
-- [centros] :: [Vector]      Lista de centros actualizada

getNewM :: [(Vector, Vector)] -> [Vector] -> [Vector]
getNewM xms [] = []
getNewM xms (m:ms) = [calculaMediaM xms m] ++ (getNewM xms ms)


-- Funcion calculaMediaM :: [(Vector, Vector)] -> Vector -> Vector
-- Recibe por parametros una lista de tuplas que asocia a cada vector
-- del dataset un centro y un centro concreto y retorna dicho centro actualizado
-- El objetivo es calcular un nuevo centro teniendo en cuenta los vectores asociados a este

-- Parametros:
-- xms :: [(Vector, Vector)]   Lista de tuplas que asocia a cada vector del dataset su centro mas cercano
-- m :: Vector                 Uno de los centros actuales
-- Resultado:
-- centros :: Vector           Centro actualizado

-- Funciones relacionadas:
-- calculaMediaMAux :: [(Vector, Vector)] -> Vector -> Int -> [Double] -> Vector   -- Funcion que itera sobre los puntos asociados a m
                                                                                   -- para calcular el nuevo centro

calculaMediaM :: [(Vector, Vector)] -> Vector -> Vector
calculaMediaM xms m = calculaMediaMAux xms m 0 (replicate (fromIntegral(length m)) 0)

calculaMediaMAux :: [(Vector, Vector)] -> Vector -> Int -> [Double] -> Vector
calculaMediaMAux [] m 0 _ = m
calculaMediaMAux [] m cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length m)]]
calculaMediaMAux (xm:xms) m cont acc = if (snd xm) == m then calculaMediaMAux xms m (cont+1) [i + j  | (i,j) <- zip (elems (fst xm)) (acc)] else calculaMediaMAux xms m cont acc


-- Funcion asocXM :: [Vector] -> [Vector] -> Distancia -> [(Vector,  Vector)]
-- Recibe por parametros una lista de vectores (correspondiente al dataset), 
-- una lista de vectores (correspondiente a los centros actuales) 
-- y una funcion distancia y retorna una lista de asociaciones de los vectores
-- a sus centros actuales

-- Parametros:
-- xs :: [Vector]             Lista de vectores del dataset
-- m :: [Vector]              Lista de centros de la ultima iteracion
-- d :: Distancia             Funcion distancia
-- Resultado:
-- centros :: [(Vector,  Vector)]   Lista de tuplas vector-centro mas cercano

-- Funciones relacionadas:
-- asocXMAux :: [Vector] -> [Vector] -> Distancia -> [(Vector, Vector)] -> [(Vector, Vector)]   -- Funcion que itera sobre los puntos del dataset y 
                                                                                                -- le va asociando a cada uno su centro mas cercano

-- getMinDist :: Vector -> [Vector] -> Distancia -> Vector -- Funcion que calcula el centro mas cercano a un punto del dataset

-- fstTuple :: (Double, Vector) -> (Double, Vector) -> Ordering   -- Funcion que dada dos tuplas devuelve un Ordering comparando
                                                                  -- por la primera componente de estas

asocXM :: [Vector] -> [Vector] -> Distancia -> [(Vector,  Vector)]
asocXM xs ms d
    | null xs || null ms = error "Lista de vectores o centros vacia"
    | otherwise = asocXMAux xs ms d []

asocXMAux :: [Vector] -> [Vector] -> Distancia -> [(Vector, Vector)] -> [(Vector, Vector)]
asocXMAux [] _ d acc = acc
asocXMAux (x:xs) ms d acc = asocXMAux xs ms d ([(x,(getMinDist x ms d))] ++ acc)

getMinDist :: Vector -> [Vector] -> Distancia -> Vector
getMinDist x ms d = snd (head (sortBy fstTuple [((d x m), m) | m <- ms])) 

fstTuple :: (Double, Vector) -> (Double, Vector) -> Ordering
fstTuple (x1,y1) (x2,y2)
    | x1 > x2 = GT 
    | otherwise = LT

-------------------------------------------------------------------------------
-- Codigo "de juguete" para pruebas unitarias
-- v1 = listaVector [2.0,0.0]
-- v2 = listaVector [10.0,20.0]
-- v3 = listaVector [2.0,1.0]
-- v4 = listaVector [15.0,7.0]
-- lv = [v1, v2, v3, v4]
-- *KMeans> kMeans 3 lv distEuclidea
-- *KMeans> kMeansCompleto 3 lv distEuclidea 
-------------------------------------------------------------------------------
