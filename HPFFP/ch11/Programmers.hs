data OperatingSystem =
       GnuPlusLinux
        | OpenBSDPlusNevermindJustBSDStill 
        | Mac
        | Windows
        deriving (Eq, Show, Enum)

data ProgLang =
       Haskell
        | Agda
        | Idris
        | PureScript 
        deriving (Eq, Show, Enum)


data Programmer = Programmer { os :: OperatingSystem
                                , lang :: ProgLang } deriving (Eq, Show)


everything :: [Programmer]
everything = [ Programmer x y | x <- [GnuPlusLinux ..], y <- [Haskell ..]]