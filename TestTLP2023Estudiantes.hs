module TestTLP2023Estudiantes where

import Data.Char
import System.IO
import StockControl

type Test = (String,[(String,String)])

tfile :: String
tfile = "TestTLP2023Estudiantes.txt"

--- EXECUTE ACTIONS ---
execCompra :: Stock -> String -> Int -> (Stock,String)
execCompra s p u = let us = retrieveStock s p in
                       if ( us == -1 )
                       then (updateStock s p u,"Compra realizada")
                       else (updateStock s p (us+u),"Compra realizada")

execVenta :: Stock -> String -> Int -> (Stock,String)
execVenta s p u = let us = retrieveStock s p in
                      if ( us < u )
                      then (s,"No se puede realizar la venta")
                      else (updateStock s p (us-u),"Venta realizada")

execUnidades :: Stock -> String -> (Stock,String)
execUnidades s p =  let us = retrieveStock s p in
                        if ( us == -1 )
                        then (s,"Producto nunca en stock");
                        else (s,"Hay "++show us++" unidad"++(if (us/=1) then "es" else ""))

execListado :: Stock -> String -> (Stock,String)
execListado s p = (s,unlines (map muestra (listStock s p)))
  where muestra :: (String,Int) -> String
        muestra    (p     ,n     )  = p++": "++(show n)

--- LOOP THROUGH THE ACTIONS OF A TEST ---
loopTest :: Stock -> Int -> [(String,String)] -> IO()
loopTest _ _ [] = putStr "Fin del test, ¡superado!\n"
loopTest s n ((input,expOut):xs)
  | accion == "compra"    = let (ns,realOut) = execCompra   s p u in checkAction ns n input expOut realOut xs
  | accion == "venta"     = let (ns,realOut) = execVenta    s p u in checkAction ns n input expOut realOut xs
  | accion == "unidades"  = let (ns,realOut) = execUnidades s p   in checkAction ns n input expOut realOut xs
  | accion == "listado"   = let (ns,realOut) = execListado  s p   in checkAction ns n input expOut realOut xs
  where tokens = words input
        accion = tokens !! 0
        p      = if (length tokens > 1) then (tokens !! 1) else ""
        u      = read(tokens !! 2)::Int

--- CHECK AN ACTION ---
checkAction :: Stock -> Int -> String -> String -> String -> [(String,String)] -> IO()
checkAction ns n input expOut realOut xs
  | expOut == realOut = loopTest ns (n+1) xs
  | otherwise         = putStr ("Discrepancia en la acción: "++(show n)++" \""++input++"\":\n-Se esperaba:\n"++expOut++"\n-Se ha obtenido:\n"++realOut++"\nTest no superado\n")
        

--- CHECK A TEST ---
checkTest (name,xs) = do { putStr ("Ejecutando el test: "++name++"\n");
                           loopTest createStock 1 xs;
                         }

--- MAIN LOOP ---
loop :: [String] -> IO ()
loop []     = putStr "End of test file\n"
loop (t:ts) = do { let test = (read t)::Test in do {
                     checkTest test;
                     loop ts;
                   }
                 }

--- FUNCTION MAIN ---
main :: IO ()
main = do { putStr ("Loading test file: " ++ tfile ++ "\n");  -- INDICAMOS QUE VAMOS A LEER EL FICHERO CON LOS TESTS
            hFlush stdout;                                    -- PROVOCA QUE LA SALIDA SE VUELQUE EN PANTALLA
            fcontent <- (readFile tfile);                     -- LEEMOS EL CONTENIDO DEL FICHERO
            let tests = (lines fcontent) in do {              -- CADA LÍNEA ES UN TEST
              loop tests;                                     -- EJECUTA EL BUCLE PRINCIPAL SOBRE LOS TESTS
            }
          }

