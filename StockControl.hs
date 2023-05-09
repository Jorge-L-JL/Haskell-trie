module StockControl where
import Data.List
import Data.Maybe (isJust)


data Stock = ROOTNODE [Stock] | INNERNODE Char [Stock] | INFONODE Int
  deriving (Show,Read,Eq)


-------------------------
-- FUNCIÓN CREATESTOCK --
-------------------------

-- FUNCIÓN QUE DEVUELVE UN STOCK VACÍO --
createStock :: Stock
createStock = ROOTNODE []
--Stoc tendrá la estructure -> ROOTNODE [INNERNODE 'a' [INFONODE 2]] 
-- A = 12

---------------------------
-- FUNCIÓN RETRIEVESTOCK --
---------------------------

-- FUNCIÓN QUE DEVUELVE EL NÚMERO DE UNIDADES DE UN PRODUCTO EN EL STOCK --
-- SI NO ESTÁ, DEBERÁ DEVOLVER -1                                       
retrieveStock :: Stock         -> String -> Int
retrieveStock (INFONODE x) [] = x
retrieveStock (INFONODE _) (_:_) = 0
retrieveStock (ROOTNODE xs) (y:ys) = foldl (\acc t -> acc + retrieveStock t (y:ys)) 0 xs
retrieveStock (INNERNODE c xs) [] = 0
retrieveStock (INNERNODE c xs) (y:ys)
  | c == y = foldl (\acc t -> acc + retrieveStock t ys) 0 xs
  | otherwise = 0



-- ## FUNCIONA 'PLATO DE POSTRE'
-- retrieveStock (INFONODE x) [] = x
-- retrieveStock (INFONODE _) (_:_) = 0
-- retrieveStock (ROOTNODE xs) (y:ys) = foldl (\acc t -> acc + retrieveStock t (y:ys)) 0 xs
-- retrieveStock (INNERNODE c xs) (y:ys)
--   | c == y = foldl (\acc t -> acc + retrieveStock t ys) 0 xs
--   | otherwise = 0






-- retrieveStock (INFONODE x) [] = x
-- retrieveStock (ROOTNODE xs) (y:ys) = foldl (\acc t -> acc + retrieveStock t (y:ys)) 0 xs
-- retrieveStock (INNERNODE c xs) (y:ys)
--   | c == y = foldl (\acc t -> acc + retrieveStock t ys) 0 xs
--   | otherwise = 0












-- retrieveStock (INFONODE n) _ = n
-- retrieveStock (ROOTNODE stocks) product = sum $ map (\s -> retrieveStock s product) stocks
-- retrieveStock (INNERNODE c stocks) product = case product of
--                                                 [] -> -1 -- Si el String está vacío, el producto no existe
--                                                 (p:ps) -> if c == p
--                                                              then retrieveStock (head stocks) ps
--                                                              else retrieveStock (ROOTNODE (tail stocks)) product
-- retrieveStock _ _ = -1




-- retrieveStock (INFONODE n) _ = n -- si el nodo es una hoja con información, devolvemos su valor
-- retrieveStock (ROOTNODE stocks) product = sum $ map (\s -> retrieveStock s product) stocks -- si es un nodo raíz, buscamos en todos los nodos hijos
-- retrieveStock (INNERNODE c stocks) product = if c == head product 
--                                                then retrieveStock (head stocks) (tail product) 
--                                                else retrieveStock (ROOTNODE (tail stocks)) product -- si coincide la letra, buscamos en el primer hijo, si no, en el resto de hijos
-- retrieveStock _ _ = -1 -- si no encontramos el producto, devolvemos -1




-------------------------
-- FUNCIÓN UPDATESTOCK --
-------------------------

-- FUNCIÓN QUE MODIFICA EL VALOR ASOCIADO A UN PRODUCTO EN EL STOCK --
-- SÓLO PUEDE ALMACENAR NÚMEROS MAYORES O IGUALES A 0               --
--
-- la función verifica si el nodo actual es una hoja o interno y la clave es vacía o no

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
updateStockList (s:ss) (p:ps) u =
  let updated = updateStock s (p:ps) u
  in if s == updated then s : updateStockList ss (p:ps) u else updated : ss
updateStockList ss [] u = ss

-----------------------
-- FUNCIÓN LISTSTOCK --
-----------------------

-- FUNCIÓN QUE DEVUELVE UNA LISTA PARES PRODUCTO-EXISTENCIA --
-- DEL CATÁLOGO QUE COMIENZAN POR LA CADENA PREFIJO p       --
--listStock :: Stock -> String -> [(String,Int)]

-- FUNCIÓN GENÉRICA DE BACKTRACKING --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    eS             c             n
  | eS n      = [n]
  | otherwise = concat (map (bt eS c) (c n))


listStock :: Stock -> String -> [(String, Int)]
listStock (ROOTNODE cs) prefix = concatMap (listStock' prefix) cs
listStock _ _ = []

listStock' :: String -> Stock -> [(String, Int)]
listStock' prefix (INFONODE n) = [(prefix, n)]
listStock' prefix (INNERNODE c cs) = concatMap (listStock' (prefix ++ [c])) cs
listStock' prefix _ = []

findPrefixNode :: Char -> [Stock] -> Maybe Stock
findPrefixNode _ [] = Nothing
findPrefixNode c (c':cs) = case c' of
  INNERNODE c'' _ | c'' == c -> Just c'
  _ -> findPrefixNode c cs

getChild :: Stock -> [Stock]
getChild (ROOTNODE cs) = cs
getChild (INNERNODE _ cs) = cs
getChild _ = []

isInfo :: Stock -> Bool
isInfo (INFONODE _) = True
isInfo _ = False

listStockPrefix :: Stock -> String -> [(String, Int)]
listStockPrefix node prefix = case filter isInfo $ bt eS getChild node of
    [] -> []
    infos -> concatMap (\(INFONODE n) -> [(prefix, n)]) infos
  where
    eS (INFONODE _) = True
    eS _ = False

