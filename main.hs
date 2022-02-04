-------------------------------------------------------------------------------
-- Autores: 
-- 	Pablo Reina Jimenez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-- 	Maria Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo tiene por objeto la lectura de datasets (.csv) para su posterior
-- uso en los distintos algoritmos implementados
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados
-- Librerias basicas
import Data.Maybe
import Data.List
import Data.Char
import Data.Array
-- Modulos para clustering y representacion
import Distancias
import KMeans
import Representacion2D
import ClusterAglomerativoListaEvolucion
import ClusterAglomerativoArbol
import Data.Tree
-- Modulos para presentacion de datos y parseo
import System.IO
import Text.CSV
import System.Directory (doesFileExist)
-- Modulos para debugging
import Debug.Trace
import Data.Typeable
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Definicion de tipos

data Dataset4Clustering  = Dataset4Clustering {
    nombre :: String, 
    cabecera :: [String],
    datos :: [Vector]
    }
    deriving (Show, Eq)                       -- Tipo dataset
-- Este tipo es usado para el procesamiento de los datos que se leen del dataset.
-- Se componene del nombre del dataset, la linea de la cabecera y el resto de lineas
-- del dataset. Estas ultimas son interpretadas como listas de vectores (Array Int Double)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Lista de funciones del modulo

-- main :: IO ()
-- Funcion principal del modulo. Se encarga de pedir el nombre del dataset y su lectura,
-- para posteriormente aplicar alguno de los algoritmos disponibles a los datos leidos

-- Resultado:
-- resultado :: IO ()                  Imprime por pantalla el resultado del algoritmo


-- Funciones relacionadas:
-- parseadorCSV :: String -> IO()      Funcion auxiliar que recibe el nombre del dataset
--                                       a leer y se ocupa de su lectura y transformacion 
--                                       al tipo Dataset4Clustering

-- fila2Array :: [String] -> Vector    Se encarga de transformas las lineas del dataset
--                                     en el tipo Vector para su posterior uso

-- seleccionAlgoritmo :: [Vector] -> IO ()
--                                            Funcion que una vez los datos han sido procesados,
--                                            se encarga de solicitar el algoritmo que se desea usar

-- seleccionaDistancia :: IO Distancia  Se encarga de solictar la funcion distancia
--                                      a usar en los distintos algortimos

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "       ELECCION DEL DATASET"
    putStrLn "--------------------------------------"
    putChar '\n'
    putStr "Introduce el nombre del fichero: "
    nombreFich <- getLine
    putChar '\n'
    exists <- doesFileExist nombreFich
    if exists then do
        putStrLn ("Leyendo fichero "++nombreFich++"...")
        putChar '\n'
        parseadorCSV nombreFich
    else do
        putStrLn "El fichero introducido no existe. Por favor, pruebe de nuevo"
        putChar '\n'
        main


parseadorCSV :: String -> IO()
parseadorCSV nombreFich = do
    input <- readFile nombreFich
    let csv = parseCSV nombreFich input
    let filas = case csv of
            (Right lineas) -> lineas
            _ -> []
    let filasValidas = filter (\x -> length x > 1) filas
    if (length filasValidas < 2) then do
        putStrLn "\n Formato de fichero incorrecto"
        main
    else do 
        let cabecera = head filasValidas
        let contenido = tail filasValidas
        let nc = length (head contenido)
        let nf = length contenido
        let dataset = Dataset4Clustering {
            nombre = nombreFich,
            cabecera = cabecera,
            datos = [ fila2Array fila | fila <- contenido ]
        }
        putStrLn (show (datos dataset))
        seleccionAlgoritmo (datos dataset)

fila2Array :: [String] -> Vector
fila2Array fila = array (1,l) filaDouble
    where   filaDouble = [ (ind, ((read dato) :: Double)) | (ind,dato) <- zip [1..] fila]
            l = length fila

seleccionAlgoritmo :: [Vector] -> IO ()
seleccionAlgoritmo datos = do  
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "       ELECCION DE ALGORITMO"
    putStrLn "--------------------------------------"
    putChar '\n'  
    putStr "Seleccione el algoritmo a usar: kMeans (KM), clusterAglomerativo (CA): "
    xs <- getLine
    if xs == "KM"
        then
            algKMeansDistancia datos
        else do
            if xs == "CA"
                then
                    clustAglomerativoDistancia datos
                else do
                    putStrLn "Introduzca una opción válida"
                    seleccionAlgoritmo datos

seleccionaDistancia :: IO Distancia
seleccionaDistancia = do
    putStr "Indique el tipo de distancia a utilizar: Euclidea (DE), Manhattan (DM) o Hamming (DH): "
    distanciaStr <- getLine
    let distancia = case distanciaStr of
            "DE" -> Just distEuclidea
            "DM" -> Just distManhattan
            "DH" -> Just distHamming
            _    -> Nothing
    if (isNothing distancia) then do
        putChar '\n'
        putStrLn "Introduzca una opcion valida"
        seleccionaDistancia
    else do
        return (fromJust distancia)


-- algKMeans :: [Vector] -> Distancia -> IO ()
-- Funcion principal del algoritmo de kMeans. Una vez recibido los datos, se encarga de 
-- solicitar los parametros del algoritmos, comprobar su validez y
-- de la posterior representacion de los resultados.

-- Parametros:     
-- datos :: [Vector]                   Lista de vectores (datos del dataset)
-- distancia :: Distancia              Funcion distancia
-- m :: Int                            Numero de centros
-- Resultado:
-- resultado :: IO ()                  Imprime por pantalla el resultado del algoritmo


-- Funciones relacionadas:
-- algKMeansDistancia :: [Vector] -> IO ()
--                                       Funcion encargada de solicitar la funcion
--                                       distancia que se va a usar en el algoritmo

--algKMeansCentros :: [Vector] -> Distancia -> IO ()
--                                       Funcion encargada de solicitar el numero
--                                       de centros que se va a usar en el algoritmo

-- obtieneCentros :: [(Vector,Vector)] -> [Vector]    
--                                       Dado una lista de puntos y sus centros asociados,
--                                       se encarga de obtener todos los centros

-- compruebaEntero :: String -> Bool     Comprueba si el String recibido puede ser 
--                                       parseado como Int

-- representaKmeansCompleto :: [(Vector,Vector)] -> [Vector] -> IO ()
--                                       Funcion usada para la representacion de los puntos asociados a cada centro

--representaAsociadosAM :: [(Vector,Vector)] -> Vector -> Int -> IO ()
--                                       Funcion que representa los puntos asociados a un centro concreto

-- representa :: [Vector] -> [(Vector, Vector)] -> IO ()
--                                       Dada una lista de vectores y otra lista de los mismos 
--                                       vectores y sus centros asociados, realiza una representacion
--                                       grafica de los puntos y los centros (solo valido para dimension 2)

algKMeans :: [Vector] -> Distancia -> Int -> IO ()
algKMeans datos distancia m = do
    putChar '\n'
    putStr  "Indique que datos desea extraer: unicamente los centros de los clusters (M), centros y datos asociados a cada uno (CM): "
    modo <- getLine
    if modo == "M"
        then do
            res <- (kMeans m datos distancia)
            let aux = (asocXM datos res distancia)
            putStrLn (show res)
            if m < 10 then
                representa res aux
            else
                return ()
        else 
            if modo == "CM"
                then do
                    res <- (kMeansCompleto m datos distancia)
                    let aux = (obtieneCentros res)
                    representaKmeansCompleto res aux
                    if m < 10 then
                        representa aux res
                    else
                        return ()
                else do
                    putChar '\n'
                    putStrLn "Introduzca un modo valido"
                    algKMeans datos distancia m

algKMeansDistancia :: [Vector] -> IO ()
algKMeansDistancia datos = do
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "       ALGORITMO DE K-MEDIAS"
    putStrLn "--------------------------------------"
    putChar '\n'
    distancia <- seleccionaDistancia
    putChar '\n'
    algKMeansCentros datos distancia

algKMeansCentros :: [Vector] -> Distancia -> IO ()
algKMeansCentros datos distancia = do
    putStr "Indique el numero de centros para el algoritmo (menor que 10 si desea una representacion grafica): "
    x <- getLine
    if compruebaEntero x then do
        let m = read x :: Int
        algKMeans datos distancia m
        
    else do
        putStrLn "Introduzca un numero de centros valido"
        algKMeansCentros datos distancia    

obtieneCentros :: [(Vector,Vector)] -> [Vector]
obtieneCentros xs = nub (map (\(x,y) -> y) xs)

compruebaEntero :: String -> Bool
compruebaEntero [] = True
compruebaEntero (x:xs) = isDigit (x) && compruebaEntero xs

representaKmeansCompleto :: [(Vector,Vector)] -> [Vector] -> IO ()
representaKmeansCompleto _ [] = return ()
representaKmeansCompleto xms (m:ms) = do
    putChar '\n'
    putChar '\n'
    putStrLn ("Puntos asociados al centro " ++ show (elems m))
    putStrLn "--------------------------------------" 
    representaAsociadosAM xms m 1
    representaKmeansCompleto xms ms

representaAsociadosAM :: [(Vector,Vector)] -> Vector -> Int -> IO ()
representaAsociadosAM [] _ _ = return ()
representaAsociadosAM (x:xs) m i = do
    if snd x == m then do
        putStrLn (show i ++ ".-  " ++ show (elems (fst x)))
        representaAsociadosAM xs m (i+1)
    else
        representaAsociadosAM xs m i



representa :: [Vector] -> [(Vector, Vector)] -> IO ()
representa m xm = do
    if snd (bounds (m!!0)) == 2
        then do
            putStr "¿Quiere una representacion grafica de los puntos: SI (S), NO (N)? "
            x <- getLine
            if x == "N"
                then
                    return ()
                else if x == "S" then
                    dibuja m xm
                else do
                    putStrLn "Introduzca una opcion valida"
                    representa m xm
    else 
        return ()

-- clustAglomerativo :: Cluster -> Distancia -> IO ()
-- Funcion principal del algoritmo de cluster aglomerativo, que dado un cluster
-- inicial (lista de vectores) y una funcion distancia ejecuta el algoritmo de 
-- cluster aglomerativo. 
-- El algoritmo comienza con un cluster por cada punto (vector). En las sucesivas 
-- iteraciones del algoritmo, se toman los dos clusters mas proximos y se fusionan.
-- El algoritmo finaliza cuando todos los vectores pertenecen al mismo cluster.
-- Permite dos modalidaes, el cluster aglomerativo de arbol
-- y de listas de evolucion

-- Parametros:     
-- datos :: Cluster                    Lista de vectores (datos del dataset)
-- distancia :: Distancia              Funcion distancia
-- Resultado:
-- resultado :: IO ()                  Imprime por pantalla el resultado del algoritmo


-- Funciones relacionadas:
-- clustAglomerativoDistancia :: Cluster -> IO ()
--                                       Funcion encargada de solicitar la funcion
--                                       distancia que se va a usar en el algoritmo


clustAglomerativo :: Cluster -> Distancia -> IO ()
clustAglomerativo datos distancia = do
    putChar '\n'
    putStr "Seleccion el tipo de estructura de datos: listaEvolucion (LE), Arbol (A): "
    xs <- getLine
    if xs == "LE"
        then do
            let d = inicializaClusteringAglomerativoLE datos
            representaClusterAglomerativoLE (clusteringAglomerativoLE distancia d)
        else do
            if xs == "A"
                then 
                    clustAglomerativoArbol distancia datos
                else do
                    putChar '\n'
                    putStrLn "Introduzca una opción válida"
                    clustAglomerativo datos distancia

clustAglomerativoDistancia :: Cluster -> IO ()
clustAglomerativoDistancia datos = do
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "ALGORITMO DE CLUSTERING AGLOMERATIVO"
    putStrLn "--------------------------------------"
    putChar '\n'
    distancia <- seleccionaDistancia
    clustAglomerativo datos distancia


-- representaClusterAglomerativoLE :: [(Int, [Cluster], Int)] -> IO ()
-- Funcion principal del algoritmo de cluster aglomerativo modelado
-- mediantes listas de evolucion

-- Parametros:     
-- xs :: [(Int, [Cluster], Int)]       Lista de tuplas de tres elementos, cuyo primer
--                                     elemento es el nivel acutal, el segundo la lista
--                                     de cluster en este momento y el tercero indica el numero
--                                     de clusters actuales (al final este numero sera 1)
-- Resultado:
-- resultado :: IO ()                  Imprime por pantalla el resultado del algoritmo


-- Funciones relacionadas:
-- representaUncluster :: [Cluster] -> Int -> IO ()
--                                       Representa los clusters en un nivel en concreto

-- fst' (a,_,_) = a                    Funciones usadas para extraer los elementos 
-- snd' (_,a,_) = a                    de tuplas de tres elementos
-- thr' (_,_,a) = a

representaClusterAglomerativoLE :: [(Int, [Cluster], Int)] -> IO ()
representaClusterAglomerativoLE [] = return ()
representaClusterAglomerativoLE (x:xs) = do
    let nivel = fst' x
    let ls = snd' x
    putChar '\n'
    putChar '\n'
    putStrLn ("Nivel " ++ show nivel ++ " de la lista de evolucion: ")
    putStrLn "--------------------------------------" 
    representaUncluster ls 1
    representaClusterAglomerativoLE xs

representaUncluster :: [Cluster] -> Int -> IO ()
representaUncluster [] _ = return ()
representaUncluster (x:xs) i = do
    putStrLn ("Cluster numero " ++ show i ++ ": " ++ show (map elems x))
    representaUncluster xs (i+1)

fst' :: (a, b, c) -> a
fst' (e,_,_) = e

snd' :: (a, b, c) -> b
snd' (_,e,_) = e

thr' :: (a, b, c) -> c
thr' (_,_,e) = e

-- clustAglomerativoArbol :: Distancia -> Cluster -> IO ()
-- Funcion principal del algoritmo de cluster aglomerativo modelado
-- mediante un dendrograma


-- Parametros:     
-- fdistancia :: Distancia      Funcion Distancia
-- datos :: Cluster             Lista de vectores (datos del dataset)
-- Resultado:
-- resultado :: IO ()                  Imprime por pantalla el resultado del algoritmo


-- Funciones relacionadas:
-- representaArbol :: Arbol -> String -> Tree String
--                                              Representa los resultados en forma de arbol
--                                              haciendo uso de la libreria Data.Tree

-- toDataTreeId :: Arbol -> Tree String         Transforma nuestra estructura de datos 
--                                              a una del tipo Data.Tree para 
--                                              poder representarla mejor

-- toDataTreeCl :: Arbol -> Tree String         Transforma nuestra estructura de datos 
--                                              a una del tipo Data.Tree para 
--                                              poder representarla mejor

clustAglomerativoArbol :: Distancia -> Cluster -> IO ()
clustAglomerativoArbol fdistancia datos = do
    putChar '\n'
    putStr "Seleccione la forma de representacion por pantalla: arbol de id (AI), arbol de clusters (AC), normal (N): "
    modo <- getLine
    let d = inicializaClusteringAglomerativoA datos 
    if modo == "AC" || modo == "AI"
        then do
            let res = clusteringAglomerativoA fdistancia d
            putStrLn ( drawTree (representaArbol res modo))
        else
            if modo == "N"
                then
                    putStrLn ( show (clusteringAglomerativoA fdistancia d))
                else do
                    putChar '\n'
                    putStrLn "Introduzca una opción válida"
                    clustAglomerativoArbol fdistancia datos

representaArbol :: Arbol -> String -> Tree String
representaArbol res modo
    | modo == "AI" = toDataTreeId res
    | modo == "AC" = toDataTreeCl res
    | otherwise = error "Modo no valido"

toDataTreeId :: Arbol -> Tree String
toDataTreeId (H id cl) = Node (show (id)) []
toDataTreeId (N id cl n1 n2 ) = Node (show (id)) [toDataTreeId n1, toDataTreeId n2]

toDataTreeCl :: Arbol -> Tree String
toDataTreeCl (H id cl) = Node (show (cl)) []
toDataTreeCl (N id cl n1 n2 ) = Node (show (cl)) [toDataTreeCl n1, toDataTreeCl n2]


