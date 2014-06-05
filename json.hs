import           Data.List (intercalate)


data JValue = JString String
            | JBool   Bool
            | JNumber Double
            | JObject [(String, JValue)]
            | JArray  [JValue]
            | JNull


compressJson :: String -> String
compressJson json = compress json False False
    -- compress params: json, inStr, aferEscape
    where compress []          _     _     = ""
          compress ('\"' : xs) inStr False = '\"' :     compress xs (not inStr) False
          compress ('\\' : xs) inStr False =            compress xs inStr       True
          compress (x    : xs) inStr True  = '\\' : x : compress xs inStr       False
          compress (x    : xs) True  False = x :        compress xs True        False
          compress (x    : xs) False _     = parse x ++ compress xs False       False

          parse c = if c `elem` " \t\n"
              then []
              else [c]


instance Show JValue where
    show JNull         = "null"
    show (JString s)   = "\"" ++ show s ++ "\""
    show (JBool True)  = "true"
    show (JBool False) = "false"
    show (JNumber n)   = show n

    show (JArray a)  = "[" ++ array a ++ "]"
        where array xs = intercalate "," (map show xs)

    show (JObject o) = "{" ++ pairs o ++ "}"
        where pairs ps    = intercalate ", " (map pair ps)
              pair (s, v) = "\"" ++ s ++ "\" : " ++ show v


main :: IO ()
main = interact compressJson
