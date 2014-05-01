import Blueprint

--reading of an initial blueprint
readBlueprintCell :: Char->Blueprintcell
readBlueprintCell 'D' = Just 'D'
readBlueprintCell 'B' = Just 'B'
readBlueprintCell 'F' = Just 'F'
readBlueprintCell 'H' = Just 'H'
readBlueprintCell '.' = Just '.'
readBlueprintCell _ = Nothing

fillLines :: [[Maybe a]]->[[Maybe a]]
fillLines xs = let n = maximum $ (map length) xs
               in map (fillUntil n) xs
    where fillUntil n ys = ys ++ (replicate (n - length ys) Nothing)

readBlueprint :: [String]->Blueprint
readBlueprint strs = Blueprint $ fillLines $ (map (map readBlueprintCell))$ (filter (not.null)) strs

readBlueprintFromFile::FilePath->IO Blueprint
readBlueprintFromFile fp = do str<-readFile fp
                              return $readBlueprint (lines str)

--reading of the script commands

readScriptCommands :: [String]->Blueprint->Blueprint
readScriptCommands strs = foldl (.) id $reverse  $ (map readScriptLine) $ (filter (not.null)) $ strs

readScriptLine :: String->Blueprint->Blueprint
readScriptLine str = assignCommand$ words str

assignCommand :: [String]->Blueprint->Blueprint
assignCommand ("repeat":"horizontal":"mirrored":"corridor":n:m:xs) = repeatedMirroredCorridor (read n) (read m)
assignCommand ("repeat":"horizontal":n:xs)  = horizontalRepeatBlueprintW (read n)
assignCommand ("repeat":"vertical":n:xs)    = verticalRepeatBlueprintW (read n)
assignCommand ("reflect":"horizontal":xs)   = horizontalMirror 
assignCommand ("reflect":"vertical":xs)     = verticalMirror
assignCommand ("fourfold":xs)               = fourfold
assignCommand ("add":"wall":"top":xs)       = addWallTop 
assignCommand ("add":"wall":"bot":xs)       = addWallBot
assignCommand ("add":"wall":"left":xs)      = addWallLeft
assignCommand ("add":"wall":"right":xs)     = addWallRight
assignCommand ("add":"corridor":"top":xs)   = addCorridorTop 
assignCommand ("add":"corridor":"bot":xs)   = addCorridorBot
assignCommand ("add":"corridor":"left":xs)  = addCorridorLeft
assignCommand ("add":"corridor":"right":xs) = addCorridorRight
assignCommand ("rotate":xs)                 = rotate90
assignCommand _                             = id
     

--reading of the whole script file
readScriptString :: String->Blueprint
readScriptString str = let (startingbp,dash:scriptcommands)=break startWithDash (lines str)
                           scriptfunctions = readScriptCommands scriptcommands
                           initialbp       = readBlueprint startingbp
                       in scriptfunctions initialbp
                    where startWithDash ('-':_) = True
                          startWithDash _       = False

readBlueprintScriptFromFile :: FilePath->IO Blueprint
readBlueprintScriptFromFile fp = do str<-readFile fp
                                    return $readScriptString str

