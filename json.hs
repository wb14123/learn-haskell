{-# LANGUAGE DeriveDataTypeable #-}

import Data.List (intercalate)
import Control.Exception
import Data.Typeable
import Debug.Hood.Observe


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
splitJson spliter json       = innerSplit' json spliter "" ""

innerSplit' :: String -> Char -> String -> String -> [String]
innerSplit'  = observe "Informative name for innerSplit"  innerSplit

innerSplit :: String -> Char -> String -> String -> [String]
innerSplit ""        _       _        word = [word]

innerSplit ('\\':xs) spliter encloser word =
    innerSplit ys spliter encloser (word ++ ['\\', y])
    where y:ys = xs

innerSplit ('\"':xs) spliter ('\"':es) word = innerSplit xs spliter es              (word ++ "\"")
innerSplit ('\"':xs) spliter encloser  word = innerSplit xs spliter ('\"':encloser) (word ++ "\"")
innerSplit (x   :xs) spliter ('\"':es) word = innerSplit xs spliter ('\"':es)       (word ++ [x])
innerSplit ('[' :xs) spliter encloser  word = innerSplit xs spliter (']' :encloser) (word ++ "[")
innerSplit ('{' :xs) spliter encloser  word = innerSplit xs spliter ('}' :encloser) (word ++ "{")

innerSplit (x   :xs) spliter ""        word = if spliter == x
    then word : innerSplit xs spliter "" ""
    else        innerSplit xs spliter "" (word ++ [x])

innerSplit (x   :xs) spliter (e:es)    word = if e == x
    then innerSplit xs spliter es     (word ++ [x])
    else innerSplit xs spliter (e:es) (word ++ [x])

removeEnclose :: String -> String
removeEnclose (_:s) = innerRemove s
    where innerRemove [_]    = ""
          innerRemove (x:xs) = x:innerRemove xs
-----------------------------------------


-----------------------------------------
-- Parse JSON
readJson :: String -> JValue
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
