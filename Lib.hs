module Lib (menu) where -- esto lo habiamos omitido por lo tanto a la hora de ejecutar el stack build salia un error ya que el main no tenia a una funcion a la cual llamar.
import Data.List
import System.IO

-- Definición de la estructura para los ítems
data Item = Item {
    nombreItem :: String,
    categoriaItem :: String
} deriving (Show, Read)

-- Registrar nuevo ítem
registrarItem :: [Item] -> String -> String -> [Item]
registrarItem inventario nombre categoria = inventario ++ [Item nombre categoria]

-- Función para buscar ítem según la categoría
buscarPorCategoria :: [Item] -> String -> [Item]
buscarPorCategoria inventario categoria = filter (\item -> categoriaItem item == categoria) inventario

-- Función para contar los ítems de una categoría
contarItemPorCategoria :: [Item] -> String -> Int
contarItemPorCategoria inventario categoria = length (buscarPorCategoria inventario categoria)

-- Función para mostrar un item
mostrarItem :: Item -> String
mostrarItem (Item nombre categoria) = "Nombre: " ++ nombre ++ ", Categoría: " ++ categoria

-- Función para listar los items en el inventario
listarItem :: [Item] -> IO ()
listarItem [] = putStrLn "No hay items en el inventario."
listarItem inventario = do
    putStrLn "items  en el inventario:"
    mapM_ (putStrLn . mostrarItem) inventario

-- Función para guardar la información de los items en un archivo de texto
guardarInventario :: [Item] -> IO ()
guardarInventario inventario = do
    withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarItem inventario))
    putStrLn "item guardado en el archivo inventario.txt."

-- Función para cargar la información de los items desde un archivo de texto
cargarInventario :: IO [Item]
cargarInventario = do
    contenido <- readFile "inventario.txt"
    let lineas = lines contenido
    return (map leerItem lineas)
    where
        leerItem linea = read linea :: Item

-- Menú
main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de gestión de inventario"
    menu []

menu :: [Item] -> IO()
menu inventario = do
    putStrLn "Selecciona una opción:"
    putStrLn "1. Ingresar un ítem nuevo"
    putStrLn "2. Buscar ítem por categoría"
    putStrLn "3. Listar todos los productos"
    putStrLn "4. Recuento de ítems por categoría"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del ítem:"
            nombre <- getLine
            putStrLn "Ingrese la categoría del ítem:"
            categoria <- getLine
            let nuevoInventario = registrarItem inventario nombre categoria
            putStrLn "Ítem registrado correctamente"
            menu nuevoInventario

        "2" -> do
            putStrLn "Ingrese la categoría que desea buscar:"
            categoria <- getLine
            let itemsEncontrados = buscarPorCategoria inventario categoria
            mapM_ (putStrLn . mostrarItem) itemsEncontrados
            menu inventario

        "3" -> do
            listarItem inventario
            menu inventario

        "4" -> do
            putStrLn "Ingrese la categoría de la que desea contar los elementos:"
            categoria <- getLine
            let cantidad = contarItemPorCategoria inventario categoria
            putStrLn ("El número de ítems en " ++ categoria ++ ": " ++ show cantidad)
            menu inventario

        "5" -> do
            putStrLn "Guardando inventario y saliendo............"
            guardarInventario inventario

        _ -> do
            putStrLn "Opción inválida, intente otra vez."
            menu inventario
 