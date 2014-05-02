module Main (main) where 

import System.Environment

import Macro
import Blueprintscript

main :: IO ()
main = do args<-getArgs
          case (length args) of
              0 -> putStrLn "No arguments given"
              1 -> do bp<-readBlueprintScriptFromFile (head args)
                      print bp
              3 -> do bp<-readBlueprintScriptFromFile (head args)
                      macrofiles (args !! 1) bp (args !! 2)
              _ -> putStrLn "Wrong number of Parameters"
