-------------------------------------------------------------------------------
-- Autores: 
-- 	Pablo Reina Jimenez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-- 	Maria Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es 
-------------------------------------------------------------------------------

module ClusterAglomerativoListaEvolucion(
    inicializaClusteringAglomerativoLE,     -- [Vector] -> EvolucionClusters
    clusteringAglomerativoLE                -- Distancia -> EvolucionClusters -> EvolucionClusters
) where

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo tiene por objeto implementar el algoritmo de clustering aglomerativo
-- modelandolo mediante una lista de evolucion.

-- El algoritmo comienza con un cluster por cada punto (vector). En las sucesivas 
-- iteraciones del algoritmo, se toman los dos clusters mas proximos y se fusionan.
-- El algoritmo finaliza cuando todos los vectores pertenecen al mismo cluster.

-- Imagen de referencia: https://editor.analyticsvidhya.com/uploads/45830agg_fig.gif
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados 
import Data.Array
import Data.Matrix
import Data.List
import Distancias
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Definicion de tipos

-- type Cluster = [Vector]              -- Recogido en Distancias.hs
type Nivel = (Int, [Cluster], Int)      -- (numIter, listClusters, numClusters) :: (Int, [Cluster], Int)
                                        -- Tipo que recoge los clusters existentes  
                                        -- en una iteracion concreta del algoritmo.
type EvolucionClusters = [Nivel]        -- Tipo que muestra la evolucion de los
                                        -- clusters a lo largo de las sucesivas
                                        -- iteraciones.

-- El modelado mediante la lista de evolucion (EvolucionClusters) consiste en que 
-- cada elemento de la lista (Nivel) recoge el resultado de una iteracion del algoritmo,
-- es decir, los clusters formados en la iteracion i-esima. 

-- Permitiendo visualizar el avance en la asociacion de clusters.
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Lista de funciones del modulo

-- Funcion inicializaClusteringAglomerativoLE :: [Vector] -> EvolucionClusters
-- Recibe la lista de los vectores (datos del dataset) y retorna el primer
-- nivel del algoritmo: todos los elementos de la lista de vectores forman 
-- un cluster por si mismos.
-- El objetivo es utilizarla como inicializacion del algoritmo de clustering 
-- (clusteringAglomerativoLE).

-- Parametros:     
-- puntosIniciales :: [Vector]              Lista de vectores del dataset
-- Resultado:
-- [Nivel 0] :: EvolucionClusters           Lista de longitud 1 que contiene
--                                          el nivel 0 (inicializacion):
--                                          [(numIter=0, listClustersIter=[[v1], [v2], ...], numClustersIter=numVectores)] 

inicializaClusteringAglomerativoLE :: [Vector] -> EvolucionClusters
inicializaClusteringAglomerativoLE puntosIniciales = [ ( 0, [ [punto] | punto <- puntosIniciales], length puntosIniciales ) ]


-- Funcion clusteringAglomerativoLE :: Distancia -> EvolucionClusters -> EvolucionClusters
-- Es la funcion base del algoritmo de clustering: obtiene la evolucion de
-- la lista de clusters.
-- Recibe una funcion distancia y va actualizando la lista de evolucion 
-- de los clusters, aplicando recursivamente iteraciones del algoritmo 
-- de clustering hasta que todos los vectores se unifiquen bajo un mismo cluster.

-- Cuando se utilice esta funcion sera inicializada con el resultado de
-- inicializaClusteringAglomerativoLE. Para aplicar una iteracion del algoritmo
-- llama a calculaSiguienteNivel y el nivel obtenido lo introduce en la lista
-- de evoluciones. 

-- Parametros:     
-- fdistancia :: Distancia                  Tipo de distancia a usar
-- listaEvolucion :: EvolucionClusters      Lista con los cambios que va 
--                                          sufriendo la lista de clusters
-- Resultado:
-- listaEvolucionNew :: EvolucionClusters   Lista con todos cambios que ha
--                                          sufrido la lista de clusters a
--                                          lo largo de las sucesivas iteracion (niveles)  
--                                          [Nivel] = [(numIter, listClustersIter, numClustersIters)]     

clusteringAglomerativoLE :: Distancia -> EvolucionClusters -> EvolucionClusters
clusteringAglomerativoLE fdistancia listaEvolucion 
    | condParada        = listaEvolucion
    | otherwise         = clusteringAglomerativoLE fdistancia (listaEvolucion ++ [ calculaSiguienteNivel fdistancia nivelActual ] )
    where   nivelActual@(numNivel, listclusters, numClusters) = last listaEvolucion
            condParada = length listclusters == 1


-- Funcion calculaSiguienteNivel :: Distancia -> Nivel -> Nivel
-- Dado un nivel toma la correspondiente lista de clusters, fusiona los 
-- dos clusters mas cercanos y devuelve el siguiente nivel.

-- Para encontrar los clusters mas proximos llama a la funcion 
-- clustersDistanciaMinima

-- Parametros:     
-- fdistancia :: Distancia                  Tipo de distancia a usar
-- nivelActual :: EvolucionClusters         nivelActual = (numIter, listClusters, numClusters)
--                                          Clusters formados hasta la iteracion
--                                          actual y vectores que los integran
-- Resultado:
-- siguienteNivel :: EvolucionClusters      Nuevo nivel obtenido

-- Funciones relacionadas:
-- eliminaCluster :: Cluster -> [Cluster] -> [Cluster]      Elimina un cluster de la lista de clusters. 
--                                                          Tras fusionar dos clusters, se elimina su
--                                                          aparicion por separado.

calculaSiguienteNivel :: Distancia -> Nivel -> Nivel
calculaSiguienteNivel fdistancia nivelActual = ( numNivel+1, listClustersNueva, length listClustersNueva )
    where   (numNivel, listclusters, numClusters) = nivelActual
            clustersMasCercanos@(c1,c2) = fst $ clustersDistanciaMinima fdistancia listclusters
            fusionClustersMasCercanos = c1 ++ c2
            listClustersNueva =  fusionClustersMasCercanos : (eliminaCluster c2 (eliminaCluster c1 listclusters))

eliminaCluster :: Cluster -> [Cluster] -> [Cluster] 
eliminaCluster cluster lclusters = prefijo ++ sufijo
    where   prefijo = takeWhile(/=cluster) lclusters 
            sufijo = drop (length prefijo + 1) lclusters
    

-- Funcion clustersDistanciaMinima :: Distancia -> [Cluster] -> ((Cluster, Cluster), Double)
-- Dada una funcion de distancia y la lista de clusters, devuelve el par 
-- (2 clusters mas cercanos, distancia entre ellos)

-- Para obtener la distancia entre pares de clusters (para poder calcular el minimo)
-- llama a calculaMatrizProximidad

-- Parametros:     
-- fdistancia :: Distancia                          Tipo de distancia a usar
-- vss :: [Cluster]                                Lista de clusters
-- Resultado:
-- ((c1,c2),dist) :: ((Cluster, Cluster), Double)   Los dos clusters mas proximos
--                                                  y la distancia entre ellos 

-- Funciones relacionadas:
-- sndTuple :: ((Cluster, Cluster), Double) -> ((Cluster, Cluster), Double) -> Ordering                                      
--                                          Comparador por el segundo elemento de la tupla   

-- transformaMatriz :: Matrix Double -> [Cluster] -> [( (Cluster, Cluster), Double )]  -- Transoforma la estructura de datos Matrix en una lista de tuplas,
                                                                                       -- cuyo primer elemento es un par de cluster y el segundo la distancia entre ambos

clustersDistanciaMinima :: Distancia -> [Cluster] -> ((Cluster, Cluster), Double)
clustersDistanciaMinima fdistancia lclusters = head(sortBy sndTuple lclustersDist)
    where   matrizProx = calculaMatrixProximidad fdistancia lclusters
            lclustersDist = transformaMatriz matrizProx lclusters

sndTuple :: ((Cluster, Cluster), Double) -> ((Cluster, Cluster), Double) -> Ordering
sndTuple (x1,y1) (x2,y2)
    | y1 > y2 = GT 
    | otherwise = LT

transformaMatriz :: Matrix Double -> [Cluster] -> [( (Cluster, Cluster), Double )]
transformaMatriz matriz lclusters = [ ((lclusters!!(i-1), lclusters!!(j-1)), getElem i j matriz) | i<- [1..n], j <- [1..n], i/=j ]
    where   n = length lclusters

----------------------------------------------------------------------
            


-- Funcion calculaMatrixProximidad :: Distancia -> [Cluster] -> Matrix Double
-- Dada una funcion distancia y la lista de clusters obtiene una matriz simetrica 
-- (optimo para no calcular cada distancia 2 veces) con la distancia entre 
-- dos clusters cualesquiera. 

-- Se define la distancia entre clusters como la distancia entre los puntos medios (centros)
-- de dos clusters. 

-- Parametros:     
-- fdistancia :: Distancia                          Tipo de distancia a usar
-- listClusters :: [Cluster]                        Lista de clusters
-- Resultado:
-- matriz :: Matrix  
--                                                  Matriz que asocia a cada dos clusters
--                                                  la distancia entre ellos 

-- Funciones relacionadas:
-- calculaMatrixProximidadAux :: Distancia -> [(Int, Cluster)] -> Matrix Double -> Matrix Double
--                                                  Funcion auxiliar que dada una matriz de 0,
--                                                  la va rellenando con la distancia entre los clusters

--recalculaMatriz :: Matrix Double -> [( ( (Int, Cluster), (Int, Cluster) ), Double)] -> Matrix Double
--                                                  Funcion que va actualizando los valores de la matriz
--                                                  calculando la distancia entre dos clusters

-- calculaDistanciasAUnCluster :: Distancia -> (Int, Cluster) -> [(Int, Cluster)] -> [( ( (Int, Cluster), (Int, Cluster) ), Double)]
--                                                  Calcula la distancia de todos los clusters
--                                                  a uno en concreto

-- distanciaEntreClusters :: Distancia -> Cluster -> Cluster -> Double
--                                                  Calcula la distancia entre dos clusters

-- calculaMedia :: Cluster -> Vector                Calcula el punto medio de cada cluster

calculaMatrixProximidad :: Distancia -> [Cluster] -> Matrix Double
calculaMatrixProximidad fdistancia listClusters = calculaMatrixProximidadAux fdistancia lindClusters matrizacc
    where   lindClusters = zip [1..] listClusters
            dim = length listClusters
            matrizacc = matrix dim dim (\(i,j) -> 0.0)

calculaMatrixProximidadAux :: Distancia -> [(Int, Cluster)] -> Matrix Double -> Matrix Double
calculaMatrixProximidadAux fdistancia lindClusters matrizacc 
    | null lindClusters = matrizacc
    | otherwise = calculaMatrixProximidadAux fdistancia restoClusters matrizaccNew
    where   headCluster = head lindClusters
            restoClusters = tail lindClusters
            distClusterActual2Resto = calculaDistanciasAUnCluster fdistancia headCluster restoClusters
            matrizaccNew = recalculaMatriz matrizacc distClusterActual2Resto

recalculaMatriz :: Matrix Double -> [( ( (Int, Cluster), (Int, Cluster) ), Double)] -> Matrix Double
recalculaMatriz matriz [] = matriz
recalculaMatriz matriz ( ( ( (nc1, c1), (nc2, c2) ), distancia ) : xss)  = recalculaMatriz matrizNew2 xss
    where   matrizNew = setElem distancia (nc1, nc2) matriz
            matrizNew2 = setElem distancia (nc2, nc1) matrizNew

calculaDistanciasAUnCluster :: Distancia -> (Int, Cluster) -> [(Int, Cluster)] -> [( ( (Int, Cluster), (Int, Cluster) ), Double)]
calculaDistanciasAUnCluster fdistancia _ [] = []
calculaDistanciasAUnCluster fdistancia clusterH (clusterS:xss) = distanciaCHaCS : avanza
    where   (nch, ch) = clusterH
            (ncs, cs) = clusterS
            distanciaCHaCS = ( (clusterH, clusterS), distanciaEntreClusters fdistancia ch cs)
            avanza = calculaDistanciasAUnCluster fdistancia clusterH xss

-- Distancia entre dos clusters cualesquiera
distanciaEntreClusters :: Distancia -> Cluster -> Cluster -> Double
distanciaEntreClusters fdistancia v1 v2 = fdistancia vm1 vm2
    where vm1 = calculaMedia v1
          vm2 = calculaMedia v2

-- Calcular el punto medio de cada cluster
calculaMedia :: Cluster -> Vector
calculaMedia v = calculaMediaAux v 0 (replicate (fromIntegral (length (elems (v!!0)))) 0)
    where   calculaMediaAux [] cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length acc)]]
            calculaMediaAux (xm:xms) cont acc = calculaMediaAux xms (cont+1) [i + j  | (i,j) <- zip (elems xm) (acc)] 

-------------------------------------------------------------------------------
-- Codigo "de juguete" para pruebas unitarias
-- v1 = listaVector [2.0,0.0]
-- v2 = listaVector [10.0,20.0]
-- v3 = listaVector [2.0,1.0]
-- v4 = listaVector [15.0,7.0]
-- lv = [v1, v2, v3, v4]
-- *ClusterAglomerativoListaEvolucion> d = inicializaClusteringAglomerativoLE lv
-- *ClusterAglomerativoListaEvolucion> clusteringAglomerativoLE distEuclidea d
-------------------------------------------------------------------------------
