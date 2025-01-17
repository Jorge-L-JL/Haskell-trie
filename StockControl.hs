module StockControl where
import Data.List
import Data.Maybe
import Data.List (sort)



data Stock = ROOTNODE [Stock] | INNERNODE Char [Stock] | INFONODE Int
  deriving (Show,Read,Eq)


-------------------------
-- FUNCIÓN CREATESTOCK --
-------------------------

-- FUNCIÓN QUE DEVUELVE UN STOCK VACÍO --
createStock :: Stock
createStock = ROOTNODE []


---------------------------
-- FUNCIÓN RETRIEVESTOCK --
---------------------------

-- FUNCIÓN QUE DEVUELVE EL NÚMERO DE UNIDADES DE UN PRODUCTO EN EL STOCK --
-- Si el resultado es Just x, se ejecuta la acción x (en este caso es retornar el valor x), 
-- si se encuentra la cadena en el trie, la función retrieveStock retornará el valor entero asociado.
retrieveStock :: Stock -> String -> Int
retrieveStock stock str = case searchTrie stock str of
  Just x -> x
  Nothing -> -1

-- Se utiliza Maybe por la sentencia case enla que utilizamos el Just y Nothing
-- El tipo Maybe en Haskell es una forma de representar la posibilidad de que un valor pueda estar presente o ausente.
-- Si el resultado es Nothing, se devuelve -1, significando que la cadena no se encuentra en el trie
searchTrie :: Stock -> String -> Maybe Int 
searchTrie stock str = case str of
  [] -> getInfo stock
  (c:cs) -> case stock of
    ROOTNODE xs -> searchInNodes xs str
    INNERNODE ch xs -> if ch == c then searchInNodes xs cs else Nothing
    INFONODE _ -> Nothing

searchInNodes :: [Stock] -> String -> Maybe Int
searchInNodes [] _ = Nothing
searchInNodes (node:nodes) str = case searchTrie node str of
  Just x -> Just x
  Nothing -> searchInNodes nodes str

getInfo :: Stock -> Maybe Int
getInfo stock = case stock of
  INFONODE x -> Just x
  _ -> Nothing


-------------------------
-- FUNCIÓN UPDATESTOCK --
-------------------------

-- FUNCIÓN QUE MODIFICA EL VALOR ASOCIADO A UN PRODUCTO EN EL STOCK --
-- SÓLO PUEDE ALMACENAR NÚMEROS MAYORES O IGUALES A 0               --
--

--
updateStock :: Stock         -> String -> Int -> Stock
updateStock s "" u = s
updateStock (INFONODE _) _ u = INFONODE u
updateStock (INNERNODE c ss) (p:ps) u =
  if c == p then INNERNODE c (updateStockList ss ps u) else INNERNODE c ss
updateStock (ROOTNODE ss) (p:ps) u = ROOTNODE (updateStockList ss (p:ps) u)

updateStockList :: [Stock] -> String -> Int -> [Stock]
updateStockList [] [] u = [INFONODE u]
updateStockList [] (p:ps) u = [INNERNODE p (updateStockList [] ps u)]
updateStockList (s@(INFONODE _):ss) [] u = INFONODE u : ss  --Ponemos en práctica el operador @ para vincular el argumento con un nombre, en este caso 's'
updateStockList (s@(INNERNODE c ss'):ss) [] u = s : updateStockList ss [] u
updateStockList (s@(INFONODE _):ss) (p:ps) u = s : updateStockList ss (p:ps) u
updateStockList (s@(INNERNODE c ss'):ss) (p:ps) u =
  if c == p then INNERNODE c (updateStockList ss' ps u) : ss
  else s : updateStockList ss (p:ps) u

-----------------------
-- FUNCIÓN LISTSTOCK --
-----------------------

-- FUNCIÓN QUE DEVUELVE UNA LISTA PARES PRODUCTO-EXISTENCIA --
-- DEL CATÁLOGO QUE COMIENZAN POR LA CADENA PREFIJO p       --
--listStock :: Stock -> String -> [(String,Int)]

--Se utiliza la función isPrefixOf (nativa de Haskell) de manera infija
--Se utiliza el operador '.' el cual sirve para realizar una composición de funciones
--Se utiliza fst, es una función de Haskell que devuelve el primer elemento de un
--Se utiliza el operador $ para permitir que el lado derecho de $ se evalúe completamente antes de aplicar la función del lado izquierdo.
--Se utiliza la función Sort para devolver los resultado ordenados
listStock :: Stock -> String -> [(String, Int)]
listStock (ROOTNODE cs) prefix = sort $ filter ((prefix `isPrefixOf`) . fst) $ concatMap (listStock' prefix "") cs
  where
    listStock' :: String -> String -> Stock -> [(String, Int)]
    listStock' prefix prefixSoFar (INFONODE n)
      | prefixSoFar `isPrefixOf` prefix = [(prefixSoFar, n)]
      | otherwise = []
    listStock' prefix prefixSoFar (INNERNODE c cs)
      | prefixSoFar `isPrefixOf` prefix =
          nub $ concatMap (listStock' prefix (prefixSoFar ++ [c])) cs ++
            [(prefixSoFar ++ [c] ++ prefix', n) | (prefix', n) <- listStock (ROOTNODE cs) (drop (length prefixSoFar) prefix)]
      | otherwise = []
    listStock' _ _ _ = []


-- FUNCIÓN GENÉRICA DE BACKTRACKING --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    eS             c             n
  | eS n      = [n]
  | otherwise = concat (map (bt eS c) (c n))


stock = ROOTNODE [INNERNODE 'b' [INNERNODE 'o' [INNERNODE 'l' [INFONODE 12],INNERNODE 't' [INNERNODE 'e' [INNERNODE 'l' [INNERNODE 'l' [INNERNODE 'a' [INNERNODE ' ' [INNERNODE '1' [INNERNODE 'l' [INFONODE 20]],INNERNODE '2' [INNERNODE 'l' [INFONODE 10]]]]]]]]]],INNERNODE 'p' [INNERNODE 'l' [INNERNODE 'a' [INNERNODE 't' [INNERNODE 'o' [INFONODE 20,INNERNODE ' ' [INNERNODE 'd' [INNERNODE 'e' [INNERNODE ' ' [INNERNODE '2' [INNERNODE ' ' [INNERNODE 'c' [INNERNODE 'o' [INNERNODE 'l' [INNERNODE 'o' [INNERNODE 'r' [INNERNODE 'e' [INNERNODE 's' [INFONODE 100]]]]]]]]],INNERNODE 'p' [INNERNODE 'o' [INNERNODE 's' [INNERNODE 't' [INNERNODE 'r' [INNERNODE 'e' [INFONODE 100]]]]]]]]]]]]]]],INNERNODE 'v' [INNERNODE 'a' [INNERNODE 's' [INNERNODE 'i' [INNERNODE 'j' [INNERNODE 'a' [INFONODE 50]],INNERNODE 't' [INNERNODE 'o' [INFONODE 10]]],INNERNODE 'o' [INFONODE 0]]]]]
--Testing
test = do
  let stock = ROOTNODE [INNERNODE 'b' [INNERNODE 'o' [INNERNODE 'l' [INFONODE 12]]]]
  let updatedStock = updateStock stock "bol" 24
  let listedItems = listStock stock "b"
  print updatedStock
  print listedItems