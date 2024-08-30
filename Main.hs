module Main where
 
import Lib (menu)
 
main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de gestión de inventario"
    menu []
