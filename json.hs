{-# LANGUAGE DeriveDataTypeable #-}

import Data.List (intercalate)
import Control.Exception
import Data.Typeable


-----------------------------------------
-- Type defination
data JValue = JString String
            | JBool   Bool
            | JNumber Double
            | JObject [(String, JValue)]
            | JArray  [JValue]
            | JNull
              deriving (Show)
-----------------------------------------


-----------------------------------------
-- Exceptions
data JException = ParseError String
    deriving (Show, Typeable)

instance Exception JException

-----------------------------------------


-----------------------------------------
-- Compress JSON
compressJson :: String -> String
compressJson json = compress json False False
    -- compress params: json, inStr, aferEscape
    where compress []          _     _     = ""
          compress ('\"' : xs) inStr False = '\"' :     compress xs (not inStr) False
          compress ('\\' : xs) inStr False = '\\' :     compress xs inStr       True
          compress (x    : xs) True  _     = x :        compress xs True        False
          compress (x    : xs) False _     = parse x ++ compress xs False       False

          parse c = if c `elem` " \t\n\v\f\r"
              then []
              else [c]
-----------------------------------------


-----------------------------------------
-- Split JSON, used in parse JSON
splitJson :: Char -> String -> [String]
splitJson spliter json       = word: remind
    where (word, remind) = innerSplit json spliter ""

innerSplit :: String -> Char -> String -> (String, [String])
-- innnerSplit: json, spliter, encloser -> (word, remind)
innerSplit ""        _       _ = ("", [])

innerSplit ('\\':xs) spliter encloser = ('\\':y:word, remind)
    where (word, remind) = innerSplit ys spliter encloser
          y:ys = xs

innerSplit ('\"':xs) spliter ('\"':es) = ('\"':word, remind)
    where (word, remind) = innerSplit xs spliter es

innerSplit ('\"':xs) spliter encloser  = ('\"':word, remind)
    where (word, remind) = innerSplit xs spliter ('\"':encloser)

innerSplit (x   :xs) spliter ('\"':es) = (x:word, remind)
    where (word, remind) = innerSplit xs spliter ('\"':es)

innerSplit ('[' :xs) spliter encloser  = ('[':word, remind)
    where (word, remind) = innerSplit xs spliter (']' :encloser)

innerSplit ('{' :xs) spliter encloser  = ('{':word, remind)
    where (word, remind) = innerSplit xs spliter ('}' :encloser)

innerSplit (x   :xs) spliter ""
    -- if we meet the spliter, start to process next word
    | spliter == x = ("", word:remind)
    | otherwise = (x:word, remind)
    where (word, remind) = innerSplit xs spliter ""

innerSplit (x   :xs) spliter (e:es) = (x:word, remind)
    -- if we meet the encloser, remove the encloser from the stack
    where (word, remind)
              | e == x = innerSplit xs spliter es
              | otherwise = innerSplit xs spliter (e:es)

removeEnclose :: String -> String
removeEnclose (_:s) = innerRemove s
    where innerRemove [_]    = ""
          innerRemove (x:xs) = x:innerRemove xs
-----------------------------------------


-----------------------------------------
-- Parse JSON
readJson :: String -> JValue
readJson "[]"      = JArray []
readJson "{}"      = JObject []
readJson ('{':xs)  = readJObjects ('{':xs)
readJson ('[':xs)  = readJArray   ('[':xs)
readJson xs        = readJValue   xs

readJObjects :: String -> JValue
readJObjects = JObject . map readJObject . splitJson ',' . removeEnclose

readJObject :: String -> (String, JValue)
readJObject = parse . splitJson ':'
    where parse [k, v] = (removeEnclose k, readJson v)
          parse other  = (throw . ParseError . show) other

readJArray :: String -> JValue
readJArray = JArray . map readJson . splitJson ',' . removeEnclose

readJValue :: String -> JValue
readJValue "true"     = JBool True
readJValue "false"    = JBool False
readJValue "null"     = JNull
readJValue ('\"': xs) = (JString . removeEnclose) ('\"':xs)
readJValue other      = readNumber (reads other :: [(Double, String)])
    where readNumber [(n, "")] = JNumber n
          readNumber _         = (throw . ParseError) other
-----------------------------------------


-----------------------------------------
-- Output JSON
showJson :: JValue -> String
showJson JNull         = "null"
showJson (JString s)   = show s
showJson (JBool True)  = "true"
showJson (JBool False) = "false"
showJson (JNumber n)   = show n

showJson (JArray a)  = "[" ++ array a ++ "]"
    where array xs = intercalate "," (map showJson xs)

showJson (JObject o) = "{" ++ pairs o ++ "}"
    where pairs ps    = intercalate "," (map pair ps)
          pair (s, v) = "\"" ++ s ++ "\":" ++ showJson v
-----------------------------------------


main :: IO ()
main = interact (showJson . readJson . compressJson)
-- main = interact compressJson
