-------------------------------------------------------------------------------
-- Autores: 
-- 	Pablo Reina Jimenez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-- 	Maria Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es 
-------------------------------------------------------------------------------

module Representacion2D(
  dibuja
) where

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo implementa la representacion grafica para el algoritmo
-- de Kmeans para datasets 2-dimensionales
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados
import CodeWorld hiding (Vector)
import Data.Array
import Distancias
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Lista de funciones del modulo

-- dibuja :: [Vector] -> [(Vector, Vector)] -> IO ()
-- Recibe una lista de clusters, lista de puntos asociadas a clusters y lista de colores. 
-- Pinta cada punto de un color asociado al cluster.
-- De maximo tantos clusters como la longitud de la lista de colores (9)

-- Parametros:     
-- m :: [Vector]                  Lista de puntos
-- xm :: [(Vector, Vector)]       Lista de pares, en el que el
--                                primer elemento es un punto
--                                y el segundo su centro asociado
-- Resultado:
-- dibujo :: IO ()                Representación gráfica de los puntos

-- Funciones relacionadas:
-- representaCentros :: [([Double], Color)] -> Picture
--                                  Representa los centros de los clusters

--dibujaCentro :: ([Double], Color) -> Picture
--                                  Representa un centro en concreto

-- representasClusters :: [(Vector, Vector)] -> [([Double], Color)] -> Picture
--                                  Representa los puntos asociados a un cluster

-- dibujaPunto :: (Vector, Vector) -> [([Double], Color)] -> Picture
--                                  Representa un punto en concreto

-- asociaColorACluster :: [Vector] -> [Color] -> [([Double], Color)]
--                                  Asocia un color a los puntos de un cluster

--  buscaColor :: [Double] -> [([Double], Color)] -> Color
--                                  Dado un punto, obtiene su color asociado

-- listaColores :: [Color]
--                                  Lista de colores (tipo Color de CodeWorld)


dibuja :: [Vector] -> [(Vector, Vector)] -> IO ()
dibuja m xm = drawingOf (representaCentros asociados & (representasClusters xm asociados))
  where asociados = asociaColorACluster m listaColores

representaCentros :: [([Double], Color)] -> Picture
representaCentros [] = blank
representaCentros (x:xs) = dibujaCentro x & representaCentros xs

dibujaCentro :: ([Double], Color) -> Picture
dibujaCentro (xs,c) = translated x y (colored black (solidRectangle 0.01 0.01))
  where x = xs !! 0
        y = xs !! 1 

representasClusters :: [(Vector, Vector)] -> [([Double], Color)] -> Picture
representasClusters [] _ = blank
representasClusters (x:xs) ls = dibujaPunto x ls & representasClusters xs ls

dibujaPunto :: (Vector, Vector) -> [([Double], Color)] -> Picture
dibujaPunto (a,b) ls = translated x1 x2 (colored (buscaColor y ls)  (solidCircle 0.01))
  where x = elems a
        y = elems b
        [x1,x2] = x

asociaColorACluster :: [Vector] -> [Color] -> [([Double], Color)]
asociaColorACluster [] _ = []
asociaColorACluster (x:xs) lc = [(elems x,head lc)] ++ asociaColorACluster xs (tail lc)

buscaColor :: [Double] -> [([Double], Color)] -> Color
buscaColor x [] = error "Lista vacia"
buscaColor x xs@(y:ys) 
  | x == fst(y) = snd(y)
  | otherwise = buscaColor x ys

listaColores :: [Color]
listaColores = [red, green, blue, pink, purple, yellow, orange, brown, gray]
