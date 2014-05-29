module SimpleJson
(
    JValue(..)
  , compressJson
) where


import           Data.List (intercalate)


data JValue = JString String
            | JBool   Bool
	    | JNumber Double
	    | JObject [(String, JValue)]
	    | JArray  [JValue]
	    | JNull


compressJson :: String -> String
compressJson json = compress json False False ""
    -- compress params: json, inStr, aferEscape, acc
    where compress []          _     _     acc = acc
          compress ('\"' : xs) inStr False acc = compress xs (not inStr) False (acc ++ "\"")
          compress ('\\' : xs) inStr False acc = compress xs inStr       True  acc
          compress (x    : xs) inStr True  acc = compress xs inStr       False (acc ++ ['\\', x])
          compress (x    : xs) True  False acc = compress xs True        False (acc ++ [x])
          compress (x    : xs) False _     acc = compress xs False       False (acc ++ parse x)

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

