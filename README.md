# Haskell-trie

FUNCIONAMIENTO retirieveStock:
La función search es una función que realiza la búsqueda de una cadena en un Trie de tipo Stock y devuelve el valor numérico correspondiente a la cadena buscada. La función toma dos argumentos: un Trie de tipo Stock y una cadena.

La función realiza la búsqueda de la cadena en el Trie, y si la cadena se encuentra en el Trie, devuelve el valor numérico correspondiente a la cadena. Si la cadena no se encuentra en el Trie, devuelve 0.

La función se implementa mediante el uso de patrones y recursión. La función toma tres patrones: (INFONODE x) [], (ROOTNODE xs) (y:ys), y (INNERNODE c xs) (y:ys).

El primer patrón, (INFONODE x) [], se aplica cuando se ha llegado al final de la cadena y se encuentra en un nodo INFONODE. En este caso, la función devuelve el valor numérico x del nodo INFONODE.

El segundo patrón, (ROOTNODE xs) (y:ys), se aplica cuando se encuentra en el nodo raíz ROOTNODE y se debe buscar en los hijos del nodo ROOTNODE para encontrar la cadena. En este caso, la función utiliza la función foldl para recorrer todos los hijos del nodo ROOTNODE y sumar el resultado de la llamada recursiva a search en cada hijo. El resultado de la función es la suma de todos los valores numéricos correspondientes a la cadena encontrada en los hijos del nodo ROOTNODE.

El tercer patrón, (INNERNODE c xs) (y:ys), se aplica cuando se encuentra en un nodo interno INNERNODE y se debe buscar en los hijos del nodo INNERNODE para encontrar la cadena. En este caso, la función utiliza la comparación c == y para verificar si el carácter actual y coincide con el carácter del nodo INNERNODE c. Si coinciden, la función utiliza la función foldl para recorrer todos los hijos del nodo INNERNODE y sumar el resultado de la llamada recursiva a search en cada hijo. El resultado de la función es la suma de todos los valores numéricos correspondientes a la cadena encontrada en los hijos del nodo INNERNODE. Si no coinciden, la función devuelve 0, ya que la cadena no se encuentra en el Trie.