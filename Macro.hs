module Macro (macrofiles) where
import Blueprint
import System.IO 
import Control.Monad


--this module maces df macros out of blueprints
--the efficiency of the macro should not rely on the blueprint
--definitions and is part of this module allone

data Macrotype = Dig | Furniture | Bed
    deriving (Show,Eq)

blueprint2Macro::Blueprint->Macrotype->String->String
blueprint2Macro bp mtype name  = (name++(show mtype)++"\n")
                                 ++ (startingPointMovements bp)
                                 ++(foldl1 (\a b -> a ++ (nextLineBeginning (n-1)) ++ (b))  (map (\c->horizontalBlueprintLine2Macro c mtype) lists))
                                 ++ "End of macro\n"
    where lists = blueprint bp
          n = length $ head lists 

--starting Point Movements
--this determines wheather there is a starting point and then moves from there to the
--upper left corner of the bp before starting

startingPointMovements :: Blueprint->String
startingPointMovements bp = case (startCoordinates bp) of 
                            Just (n,m)  ->concat (replicate n cu) ++ concat (replicate m cl)
                            Nothing     ->""

--end of group string that is needed a lot
eog :: String
eog ="\n\tEnd of group\n"

cl::  String
cl = "\t\tCURSOR_LEFT" ++ eog

cr::  String
cr = "\t\tCURSOR_RIGHT" ++ eog

cd::  String
cd = "\t\tCURSOR_DOWN" ++ eog

cu::  String
cu = "\t\tCURSOR_UP" ++ eog

leavescreen :: String
leavescreen = "\t\tLEAVESCREEN" ++ eog

select:: String
select = "\t\tSELECT" ++ eog

--building curser is used for navigation when placing furnature
--it uses build farmplot as a hack  because the is alwats possible (does not require an item
--like door or bed)
buildingCurser :: String
buildingCurser = "\t\tHOTKEY_BUILDING_FARMPLOT" ++ eog

blueprintcell2DigMacro :: Blueprintcell -> String
blueprintcell2DigMacro Nothing   = "" 
blueprintcell2DigMacro (Just _)= "\t\tSELECT"++eog++"\t\tSELECT"++eog

blueprintcell2FurnitureMacro :: Blueprintcell-> String
blueprintcell2FurnitureMacro Nothing    = ""
blueprintcell2FurnitureMacro (Just 'D') = leavescreen++"\t\tHOTKEY_BUILDING_DOOR"++eog
                                          ++select++select
                                          ++buildingCurser++leavescreen++buildingCurser
blueprintcell2FurnitureMacro (Just 'B') = leavescreen++"\t\tHOTKEY_BUILDING_BED"++eog
                                          ++select++select
                                          ++buildingCurser++leavescreen++buildingCurser
blueprintcell2FurnitureMacro (Just 'F') = leavescreen++"\t\tHOTKEY_BUILDING_CABINET"++eog
                                          ++select++select
                                          ++buildingCurser++leavescreen++buildingCurser
blueprintcell2FurnitureMacro (Just 'H') = leavescreen++"\t\tHOTKEY_BUILDING_BOX"++eog
                                          ++select++select
                                          ++buildingCurser++leavescreen++buildingCurser
blueprintcell2FurnitureMacro (Just _) = ""

blueprintcell2BedroomMacro :: Blueprintcell -> String
blueprintcell2BedroomMacro (Just 'B') = "\t\tBUILDJOB_BED_SIZE" ++ eog ++ select
blueprintcell2BedroomMacro _          = ""

horizontalBlueprintLine2Macro :: [Blueprintcell]->Macrotype -> String
horizontalBlueprintLine2Macro bpline mtype
            |mtype == Dig       = foldl1 (\a b->a ++ (cr ++ (b))) (map blueprintcell2DigMacro bpline)
            |mtype == Furniture = foldl1 (\a b->a ++ (cr ++ (b))) (map blueprintcell2FurnitureMacro bpline)
            |mtype == Bed       = foldl1 (\a b->a ++ (cr ++ (b))) (map blueprintcell2BedroomMacro bpline)

nextLineBeginning :: Int->String
nextLineBeginning n = cd ++ (concat $ replicate n cl) 

--blueprint to macro file

macrofiles :: FilePath->Blueprint->String->IO ()
macrofiles macrofolder bp macroname = do writeFile (macrofolder++macroname++(show Dig)++".mak") (blueprint2Macro bp Dig macroname)
                                         writeFile (macrofolder++macroname++(show Furniture)++".mak") (blueprint2Macro bp Furniture macroname)
                                         when (hasBeds bp) $writeFile (macrofolder++macroname++(show Bed)++".mak") (blueprint2Macro bp Bed macroname)
