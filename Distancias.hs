-------------------------------------------------------------------------------
-- Autores: 
-- 	Pablo Reina Jimenez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-- 	Maria Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es 
-------------------------------------------------------------------------------

module Distancias
(   distEuclidea,       -- Vector -> Vector -> Double
    distManhattan,      -- Vector -> Vector -> Double
    distHamming,        -- Vector -> Vector -> Double
    listaVector,        -- [Double] -> Vector
    Vector,
    Cluster,
    Distancia
) where

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo tiene por objeto definir las distintas funciones de distancia
-- que pueden ser utilizadas para los algoritmos de clustering y de k-means.
-- Se definen asi mismo los tipos basicos: vector y funcion distancia.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados
import Data.Array
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Definicion de tipos

type Vector = Array Int Double                  -- Tipo para los datos del dataset. 
                                                -- Vectores con valores reales.
type Cluster = [Vector]                         -- Tipo para los clusters utilizado
                                                -- en los algoritmos de clustering
                                                -- aglomerativo
type Distancia = Vector -> Vector -> Double     -- Tipo para las funciones de distancia 

-- El tipo Distancia pasar por parametro las funciones distancias a otras
-- funciones simplificando las signaturas de las mismas.
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Lista de funciones del modulo

-- Funcion listaVector :: [Double] -> Vector
-- Recibe por parametro una lista con los valores de cada una de las coordenadas 
-- y retorna el correspondiente vector. 

-- Parametros:     
-- xs :: [Double]           Lista de valores 
-- Resultado:
-- Vector                   Vector con valores reales


listaVector :: [Double] -> Vector
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]


-- Funcion distancia: distEuclidea, distManhattan, distHamming :: Vector -> Vector -> Double
-- Recibe por parametros dos vectores reales y retornan la distancia entre ambos.

-- Parametros:     
-- v1 :: Vector                             Vector con valores reales
-- v2 :: Vector                             Vector con valores reales
-- Resultado:
-- distancia :: Double                      Distancia entre ambos vectores


-- Distancia Euclidea: raiz cuadrada de la suma de la diferencia al cuadrado entre componentes 
distEuclidea :: Distancia
distEuclidea v1 v2 
    | indices v1 == indices v2 = sqrt (sum[(x - y)**2 | (x,y) <- zip (elems v1) (elems v2)])
    | otherwise = error "Vectores incompatibles"

-- Distancia Manhattan: suma del valor absoluto de la diferencia entre componentes
distManhattan :: Distancia
distManhattan v1 v2 
    | indices v1 == indices v2 = sum[abs (x - y) | (x,y) <- zip (elems v1) (elems v2)]
    | otherwise = error "Vectores incompatibles"

-- Distancia Hamming: numero de componentes distintas
distHamming :: Distancia
distHamming v1 v2 
    | indices v1 == indices v2 = sum[if x == y then 0 else 1 | (x,y) <- zip (elems v1) (elems v2)]
    | otherwise = error "Vectores incompatibles"