# Aprendizaje Automático. Clustering

Autores: 
-	Pablo Reina Jiménez.            Datos de contacto: pabreijim1, pabreijim1@alum.us.es  
-	María Lourdes Linares Barrera.  Datos de contacto: marlinbar, marlinbar@alum.us.es  

## **Lanzamiento del proyecto**
Para lanzar el proyecto contamos con dos opciones.

### Utilizando el intérprete de Haskell  
Esta es la opción más recomendada. Debemos seguir los siguientes pasos:  
1. Abrir el fichero main.hs  
2. Load GHCi desde un terminal en Visual Studio (ghci “ruta del archivo” o Ctrl+Alt+L)  
3. Desde el terminal escribimos main y el programa comenzará a ejecutarse
  
  
### Opción compilada
Debemos seguir los siguientes pasos:  
1. Evaluar el programa: $ runhaskell main.hs  
2. Compilar el programa: ghc main.hs -o main  
3. Ejecutar el programa: ./main  

___  

## **Testing sobre datasets**  

Tras cargar el programa se nos mostrará por línea de comandos un menú en el que podremos ir navegando por los distintos algoritmos que ofrece el proyecto y las distintas opciones de representación de los resultados.

Debemos seguir los siguientes pasos:
1. En primer lugar se nos solicitará el dataset a leer. Se recomienda tener en cuenta los datasets recomendados para cada algoritmo (pdf).
2. Selección del algoritmo: KM (kMeans) y CA (clustering aglomerativo)
    1.  Selección de distancia a utilizar DE (Euclídea), DM (Manhattan), DH (Hamming)
    2.  Si la lección es KM: solicitará el número de centros y información que desea obtener (centros y/o clusters). Si el dataset es 2-dimensional podrá solicitar la representación gráfica de los resultados (recuerde hacer zoom para ver el gráfico).
    3.  Si la lección es CA: solicitará modelo que desea utilizar LE (lista evolución) o A (árbol). En el primer caso le mostrará los resultados de forma directa. En el segundo caso le solicitará una forma de visualización del árbol. La más gráfica es el árbol de ids (AI).  

Ejemplos recomendados:  

* Ejemplo de testeo de clustering aglomerativo (modelo árbol)  
~~~
Introduce el nombre del fichero: ejemplos/user_knowledge.csv   
Seleccione el algoritmo a usar: kMeans (KM), clusterAglomerativo (CA): CA
Indique el tipo de distancia a utilizar: Euclidea (DE), Manhattan (DM) o Hamming (DH): DE
Seleccion el tipo de estructura de datos: listaEvolucion (LE), Arbol (A): A
Seleccione la forma de representacion por pantalla: arbol de id (AI), arbol de clusters (AC), normal (N): AI
~~~  

* Ejemplo de testeo de clustering aglomerativo (modelo lista de evolución)  
~~~
Introduce el nombre del fichero: ejemplos/iris_dataset.csv   
Seleccione el algoritmo a usar: kMeans (KM), clusterAglomerativo (CA): CA
Indique el tipo de distancia a utilizar: Euclidea (DE), Manhattan (DM) o Hamming (DH): DE
Seleccion el tipo de estructura de datos: listaEvolucion (LE), Arbol (A): LE
~~~ 

* Ejemplo de testeo de k-means  
~~~
Introduce el nombre del fichero: ejemplos/wine_clustering.csv
Seleccione el algoritmo a usar: kMeans (KM), clusterAglomerativo (CA): KM
Indique el tipo de distancia a utilizar: Euclidea (DE), Manhattan (DM) o Hamming (DH): DE
Indique el numero de centros para el algoritmo (menor que 10 si desea una representacion grafica): 3
Indique que datos desea extraer: unicamente los centros de los clusters (M), centros y datos asociados a cada uno (CM): M
¿Quiere una representacion grafica de los puntos: SI (S), NO (N)? S (abrir link)
~~~ 

~~~
Introduce el nombre del fichero: ejemplos/user_knowledge2c.csv
Seleccione el algoritmo a usar: kMeans (KM), clusterAglomerativo (CA): KM
Indique el tipo de distancia a utilizar: Euclidea (DE), Manhattan (DM) o Hamming (DH): DE
Indique el numero de centros para el algoritmo (menor que 10 si desea una representacion grafica): 3
Indique que datos desea extraer: unicamente los centros de los clusters (M), centros y datos asociados a cada uno (CM): M
¿Quiere una representacion grafica de los puntos: SI (S), NO (N)? S (abrir link)
~~~ 

*Observación: Debido a la gran cantidad de datos con los que trabajamos, es posible que, a veces, aparezca un error similar al siguiente: "Exception in blank-canvas application: thread blocked indefinitely in an STM transaction". En ese caso, basta pulsar enter y sigue la ejecución del programa.*  
___  

## **Librerías**

Instalación de librerías no básicas utilizadas:

~~~
cabal install matrix array csv random
cabal install codeworld-api (puede ser necesario añadir --force-reinstalls)
~~~