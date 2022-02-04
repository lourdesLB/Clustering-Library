-------------------------------------------------------------------------------
-- Autores: 
-- 	Pablo Reina Jimenez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-- 	Maria Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es 
-------------------------------------------------------------------------------

module ClusterAglomerativoArbol 
    (
    inicializaClusteringAglomerativoA,
    clusteringAglomerativoA,
    Arbol (H, N)
    ) where 

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo tiene por objeto implementar el algoritmo de clustering aglomerativo
-- modelandolo mediante un dendrograma.

-- El algoritmo comienza con un cluster por cada punto (vector). En las sucesivas 
-- iteraciones del algoritmo, se toman los dos clusters mas proximos y se fusionan.
-- El algoritmo finaliza cuando todos los vectores pertenecen al mismo cluster.

-- Imagen de referencia: https://estrategiastrading.com/wp-content/uploads/2019/07/ejemplo_dendrograma.png
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados
import Data.Array
import Data.List
import Data.Maybe
import Distancias
---------------------------------

-------------------------------------------------------------------------------
-- Definicion de tipos

-- type Cluster = [Vector]              -- Recogido en Distancias.hs        
type IdCluster = [Int]                  -- Tipo para identificar a un cluster en el arbol
-- IdCluster = lista de indices de los clusters de nivel inferior contenidos en ese cluster
-- Como inicialmente los clusters son los puntos, el identificador tambien indica los vectores 
-- que agrupa ese cluster.
-- Permite que se pueda visualizar de manera correcta la formacion del dendrograma (visualizar
-- todos los vectores en cada nivel haria ilegible el arbol)

data Arbol = H IdCluster Cluster | N IdCluster Cluster Arbol Arbol  
    deriving Eq                         -- Tipo "arbol"
-- Este tipo permite reflejar como se van uniendo los clusters. Redefinimos la funcion show 
-- para que solo muestre el id de los subclusters (nodos) por legibilidad del arbol. 
instance Show Arbol where 
    show (H id cluster)= "(H " ++ (show id) ++ ")" 
    show (N id cluster denizq dender) = "(N "++ (show id) ++ " "++ show (denizq) ++ " "++ show (dender) ++ ")"

type Dendrogram = [ Arbol ]              -- Tipo "bosque"
-- Inicialmente, todos los vectores forman un cluster por si mismos (al principio solo hay hojas sin emparejar).
-- Conforme se van fusionando los clusters se van formando arboles en paralelo (bosque).
-- Hasta el final no se obtiene un único arbol como tal.
--------------------------------

-------------------------------------------------------------------------------
-- Lista de funciones "herramienta" del modulo

-- Funcion listaClustersActuales :: Dendrogram -> [(IdCluster, Cluster)]
--         listaClustersActuales2 :: Dendrogram -> [Cluster]
-- Dado un dendrograma obtiene el ultimo estado de los clusters. Tomar el ultimo estado es, 
-- en definitiva, tomar las raices de todos los arboles del dendrograma. 

-- Parametros:
-- dendrograma :: Dendrogram                  Bosque de dependencia entre clusters
-- Resultado:
-- [(IdCluster, Cluster)]  / [Cluster]      Lista de clusters formados 

listaClustersActuales :: Dendrogram -> [(IdCluster, Cluster)]
listaClustersActuales [] = []
listaClustersActuales (arbol: xs) = obtenCluster arbol : listaClustersActuales xs
    where  obtenCluster (N id cluster izq der) = (id, cluster)
           obtenCluster (H id cluster) = (id, cluster)

listaClustersActuales2 :: Dendrogram -> [Cluster]
listaClustersActuales2 [] = []
listaClustersActuales2 (arbol: xs) = obtenCluster arbol : listaClustersActuales2 xs
    where  obtenCluster (N id cluster izq der) = cluster
           obtenCluster (H id cluster) = cluster


-- Funcion arbolAsociadoACluster :: Dendrogram -> Cluster -> Maybe Arbol
-- Funcion que dado un cluster = [Vector] devuelve el arbol del dendrograma asociado a 
-- ese cluster (arbol que tiene como raiz ese cluster)

-- Parametros:
-- Dendrogram :: Dendrogram               Bosque de dependencia entre clusters
-- cluster :: cluster                   Cluster 
-- Resultado:
-- arbol :: Maybe Arbol                 Arbol asociado a ese cluster

-- Funciones asociadas:
-- datosClusterFromArbol :: Arbol -> (IdCluster, Cluster)   
--                                      Funcion que dado un arbol devuelve su cluster asociado 
--                                      (Identificador de la raiz, cluster raiz) 

arbolAsociadoACluster :: Dendrogram -> Cluster -> Maybe Arbol
arbolAsociadoACluster [] cluster = Nothing
arbolAsociadoACluster (arbol: xs) cluster = if (clusterArbol == cluster) 
                                                then Just arbol 
                                            else 
                                                arbolAsociadoACluster xs cluster
    where (idNodo, clusterArbol) = datosClusterFromArbol arbol


datosClusterFromArbol :: Arbol -> (IdCluster, Cluster)
datosClusterFromArbol (N idCluster cluster izq der) = (idCluster, cluster)
datosClusterFromArbol (H idCluster cluster) = (idCluster, cluster)


-------------------------------------------------------------------------------
-- Lista de funciones "del algoritmo" del modulo

-- Funcion inicializaClusteringAglomerativoA :: [Vector] -> Dendrogram
-- Obtiene el primer nivel a partir de los datos: todos los elementos forman
-- un cluster por si mismos (al principio solo hay hojas sin emparejar).
-- El objetivo es utilizarla como inicializacion del algoritmo de clustering 
-- (clusteringAglomerativoA).

-- Parametros:
-- puntosIniciales :: [Vector]              Lista de vectores del dataset
-- Resultado:
-- dendrograma :: Dendrogram                  Bosque de hojas (tantas hojas como
--                                          puntos).

inicializaClusteringAglomerativoA :: [Vector] -> Dendrogram
inicializaClusteringAglomerativoA puntosIniciales = [ (H [indice] [punto] ) | (indice, punto) <- zip [0..] puntosIniciales ]


-- Funcion clusteringAglomerativoA :: Distancia -> Dendrogram -> Arbol
-- Es la funcion base del algoritmo de clustering: obtiene un arbol que 
-- refleja como se han ido fusionando los clusters.
-- Recibe una funcion distancia y va actualizando el dendrograma, aplicando 
-- recursivamente iteraciones del algoritmo de clustering hasta que 
-- todos los clusters (subarboles) se unifiquen en uno solo 
-- (cuando el dendrograma contenga un unico arbol).

-- Cuando se utilice esta funcion sera inicializada con el resultado de
-- inicializaClusteringAglomerativoA. Para aplicar una iteracion del algoritmo
-- llama a calculaSiguienteNivel.

-- Parametros:     
-- fdistancia :: Distancia                  Tipo de distancia a usar
-- Dendrogram :: Dendrogram                   Bosque de dependencia entre clusters
-- Resultado:
-- arbol :: Arbol                           Arbol de dependencia entre clusters

clusteringAglomerativoA :: Distancia -> Dendrogram -> Arbol
clusteringAglomerativoA fdistancia dendrogram 
    | condParada        = head $ dendrogram
    | otherwise         = clusteringAglomerativoA fdistancia (calculaSiguienteNivel fdistancia dendrogram)
    where   condParada = length dendrogram == 1 -- Ya solo tenemos un arbol, hemos terminado de agrupar


-- Funcion calculaSiguienteNivel :: Distancia -> Dendrogram -> Dendrogram
-- Dado el dendrograma, extrae el ultimo estado de los clusters, fusiona los 
-- dos clusters mas cercanos y devuelve el siguiente nivel.

-- Para encontrar los clusters mas proximos llama a la funcion 
-- clustersDistanciaMinima

-- Parametros:     
-- fdistancia :: Distancia                  Tipo de distancia a usar
-- dendrograma :: Dendrogram                  Lista de arboles con los clusters formados 
--                                          hasta la iteracion actual
-- Resultado:
-- siguienteNivel :: Dendrogram              Nuevo dendrograma obtenido

-- Funciones relacionadas:
-- eliminaCluster :: Cluster -> [Cluster] -> [Cluster]      Tras fusionar dos arboles (clusters), se 
--                                                          elimina su aparicion por separado en el dendrograma.

calculaSiguienteNivel :: Distancia -> Dendrogram -> Dendrogram
calculaSiguienteNivel fdistancia dendrogram = newDendrogram
    where   clustersMasCercanos@(c1,c2) = fst $ clustersDistanciaMinima fdistancia dendrogram
            (c1Id, c1list) = datosClusterFromArbol c1
            (c2Id, c2list) = datosClusterFromArbol c2
            newClusterId = c1Id ++ c2Id
            newClusterList = c1list ++ c2list
            fusionClustersMasCercanos = (N newClusterId newClusterList c1 c2)
            newDendrogram = fusionClustersMasCercanos : (eliminaCluster c2 (eliminaCluster c1 dendrogram))
        
eliminaCluster :: Arbol -> Dendrogram -> Dendrogram
eliminaCluster arbol larboles = prefijo ++ sufijo
    where   prefijo = takeWhile(/=arbol) larboles 
            sufijo = drop (length prefijo + 1) larboles

-- Funcion clustersDistanciaMinima :: Distancia -> [Arbol] -> ((Arbol, Arbol), Double)
-- Dada una funcion de distancia y la lista de arboles (del que se pueden extraer
-- los clusters formados), devuelve el par 
-- (2 clusters -vistos como subarboles- mas cercanos, distancia entre ellos)

-- Para obtener la distancia entre pares de clusters (para poder calcular el minimo)
-- llama a calculaMatrixProximidad

-- Parametros:     
-- fdistancia :: Distancia                          Tipo de distancia a usar
-- d :: Dendrogram =  [Arbol]                        Lista de arboles con los clusters formados 
--                                                  hasta la iteracion actual
-- Resultado:
-- ((c1,c2),dist) :: ((Arbol, Arbol), Double)       Los dos clusters/subarboles mas proximos
--                                                  y la distancia entre ellos 

-- Funciones relacionadas:
-- sndTuple :: ((Arbol, Arbol), Double) -> ((Arbol, Arbol), Double) -> Ordering                                  
--                                          Comparador por el segundo elemento de la tupla                                                            

clustersDistanciaMinima :: Distancia -> [Arbol] -> ((Arbol, Arbol), Double)
clustersDistanciaMinima fdistancia d = head(sortBy sndTuple (calculaMatrixProximidad fdistancia d))

sndTuple :: ((Arbol, Arbol), Double) -> ((Arbol, Arbol), Double) -> Ordering
sndTuple (x1,y1) (x2,y2)
    | y1 > y2 = GT 
    | otherwise = LT


-- Funcion calculaMatrixProximidad :: Distancia -> [Arbol] -> [((Arbol, Arbol), Double)]
-- Dada una funcion distancia y la los clusters actuales (dados a traves del bosque) 
-- obtiene una "matriz", es decir, una lista que a cadas dos clusters/arboles le asocia 
-- la distancia entre estos 

-- Se define la distancia entre clusters como la distancia entre los puntos medios (centros)
-- de dos clusters. 

-- Parametros:     
-- fdistancia :: Distancia                          Tipo de distancia a usar
-- vss :: [Arbol]                                   Lista de arboles de la que se puede extreaer
--                                                  los clusters formados hasta la iteracion actual
-- Resultado:
-- [((c1,c2),dist)] :: [((Arbol, Arbol), Double)] 
--                                                  "Matriz" que asocia a cada dos clusters (raiz de los
--                                                  arboles) la distancia entre ellos 

-- Funciones relacionadas:
-- calculaDistanciasAUnCluster :: Distancia -> Arbol -> [Arbol] -> [( (Arbol, Arbol), Double)]
--                                                  Calcula la distancia de todos los clusters
--                                                  a uno en concreto

-- distanciaEntreClusters :: Distancia -> Cluster -> Cluster -> Double
--                                                  Calcula la distancia entre dos clusters

-- calculaMedia :: Cluster -> Vector                Calcula el punto medio de cada cluster



-- Obtiene una matriz simetrica que devuelve la distancia entre dos clusters cualesquiera optimo (simetrica)
calculaMatrixProximidad :: Distancia -> [Arbol] -> [((Arbol, Arbol), Double)]
calculaMatrixProximidad fdistancia [] = []
calculaMatrixProximidad fdistancia (vs:vss) = calculaDistanciasAUnCluster fdistancia vs vss ++ (calculaMatrixProximidad fdistancia vss)

-- Distancia de todos los clusters a uno en concreto
calculaDistanciasAUnCluster :: Distancia -> Arbol -> [Arbol] -> [( (Arbol, Arbol), Double)]
calculaDistanciasAUnCluster fdistancia arbolH [] = []
calculaDistanciasAUnCluster fdistancia arbolH (arbolS:xss) = distanciaHaS: avanza
    where   (idH, clusterH) = datosClusterFromArbol arbolH
            (idS, clusterS) = datosClusterFromArbol arbolS
            distanciaHaS =  ((arbolH, arbolS), distanciaEntreClusters fdistancia clusterH clusterS)
            avanza = calculaDistanciasAUnCluster fdistancia arbolH xss

-- Distancia entre dos clusters 
distanciaEntreClusters :: Distancia -> Cluster -> Cluster -> Double
distanciaEntreClusters fdistancia v1 v2 = fdistancia vm1 vm2
    where vm1 = calculaMedia v1
          vm2 = calculaMedia v2


-- Calcular el punto medio (centro) de cada cluster
calculaMedia :: Cluster -> Vector
calculaMedia v = calculaMediaAux v 0 (replicate (fromIntegral (length (elems (v!!0)))) 0)
    where 
        calculaMediaAux [] cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length acc)]]
        calculaMediaAux (xm:xms) cont acc = calculaMediaAux xms (cont+1) [i + j  | (i,j) <- zip (elems xm) (acc)] 


-------------------------------------------------------------------------------
-- Codigo "de juguete" para pruebas unitarias
-- v1 = listaVector [2.0,0.0]
-- v2 = listaVector [10.0,20.0]
-- v3 = listaVector [2.0,1.0]
-- v4 = listaVector [15.0,7.0]
-- lv = [v1, v2, v3, v4]
-- *ClusterAglomerativoArbol> d = inicializaClusteringAglomerativoA lv
-- *ClusterAglomerativoArbol> clusteringAglomerativoA distEuclidea d
-------------------------------------------------------------------------------
