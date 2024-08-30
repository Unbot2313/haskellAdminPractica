import System.IO
import System.Exit (exitSuccess)
import Control.Exception (bracket)
import System.Directory (doesFileExist)

data Producto = Producto
    { nombre    :: String
    , categoria :: String
    , cantidad  :: Int
    } deriving (Show, Read)

type Inventario = [Producto]

-- Lee el inventario desde el archivo
leerInventario :: FilePath -> IO Inventario
leerInventario path = do
    existe <- doesFileExist path
    if existe
        then bracket (openFile path ReadMode) hClose (\handle -> do
                contenido <- hGetContents handle
                return $! read contenido)  -- Evaluación estricta para evitar el problema de lectura diferida
        else return []

-- Guarda el inventario en el archivo
guardarInventario :: FilePath -> Inventario -> IO ()
guardarInventario path inventario = bracket (openFile path WriteMode) hClose (\handle -> hPrint handle inventario)

-- Añade un producto al inventario
agregarProducto :: Producto -> Inventario -> Inventario
agregarProducto p inventario = inventario ++ [p]

-- Busca productos por categoría
buscarPorCategoria :: String -> Inventario -> [Producto]
buscarPorCategoria cat = filter (\p -> categoria p == cat)

-- Cuenta productos en una categoría
contarProductosEnCategoria :: String -> Inventario -> Int
contarProductosEnCategoria cat inventario = length $ buscarPorCategoria cat inventario

-- Muestra el menú y maneja las opciones
menu :: IO ()
menu = do
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"
    putStrLn "Seleccione una opción:"
    putStrLn "1. Registrar entrada de producto"
    putStrLn "2. Buscar productos por categoría"
    putStrLn "3. Listar todos los productos"
    putStrLn "4. Contar productos por categoría"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> registrarProducto
        "2" -> buscarPorCategoriaMenu
        "3" -> listarProductos
        "4" -> contarPorCategoriaMenu
        "5" -> do
            putStrLn "¡Hasta luego!"
            exitSuccess
        _ -> do
            putStrLn "Opción inválida, por favor intente de nuevo."
            menu

-- Función para registrar un producto
registrarProducto :: IO ()
registrarProducto = do
    putStrLn "Ingrese el nombre del producto:"
    nombreProducto <- getLine
    putStrLn "Ingrese la categoría del producto:"
    categoriaProducto <- getLine
    putStrLn "Ingrese la cantidad del producto:"
    cantidadStr <- getLine
    let cantidadProducto = read cantidadStr :: Int
    let producto = Producto nombreProducto categoriaProducto cantidadProducto
    inventario <- leerInventario "inventario.txt"
    let nuevoInventario = agregarProducto producto inventario
    guardarInventario "inventario.txt" nuevoInventario
    putStrLn $ "Producto \"" ++ nombreProducto ++ "\" registrado en la categoría \"" ++ categoriaProducto ++ "\" con cantidad " ++ show cantidadProducto ++ "."
    menu

-- Función para buscar productos por categoría
buscarPorCategoriaMenu :: IO ()
buscarPorCategoriaMenu = do
    putStrLn "Ingrese la categoría para buscar:"
    categoriaBusqueda <- getLine
    inventario <- leerInventario "inventario.txt"
    let productosEncontrados = buscarPorCategoria categoriaBusqueda inventario
    if null productosEncontrados
        then putStrLn "No se encontraron productos en esta categoría."
        else do
            putStrLn "Productos encontrados:"
            mapM_ print productosEncontrados
    menu

-- Función para listar todos los productos
listarProductos :: IO ()
listarProductos = do
    putStrLn "Lista de todos los productos:"
    inventario <- leerInventario "inventario.txt"
    mapM_ print inventario
    menu

-- Función para contar productos por categoría
contarPorCategoriaMenu :: IO ()
contarPorCategoriaMenu = do
    putStrLn "Ingrese la categoría para contar productos:"
    categoriaContar <- getLine
    inventario <- leerInventario "inventario.txt"
    let cantidadCategoria = contarProductosEnCategoria categoriaContar inventario
    putStrLn $ "Cantidad de productos en la categoría \"" ++ categoriaContar ++ "\": " ++ show cantidadCategoria
    menu

-- Función principal
main :: IO ()
main = do
    -- Asegurarse de que el archivo de inventario exista
    existe <- doesFileExist "inventario.txt"
    if not existe
        then writeFile "inventario.txt" "[]"
        else return ()
    menu
