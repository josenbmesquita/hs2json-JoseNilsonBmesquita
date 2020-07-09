-- file: ch05/PrettyJSON.hs
module Prettify where

import SimpleJSON
import Prettify

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

-- file: src/Prettify.hs
module Prettify where

import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

-- file: src/Prettify.hs
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

-- file: src/PrettyJSON.hs
pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))

-- file: src/PrettyJSON.hs
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

-- file: src/Prettify.hs
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

-- file: rwhptbr/Ch11.hs
import Prelude hiding ((<>))

-- file: src/Prettify.hs
hcat :: [Doc] -> Doc
hcat xs = undefined

-- file: rwhptbr/Ch11.hs
import Prelude hiding ((<>))

-- file: src/Prettify.hs
hcat :: [Doc] -> Doc
hcat xs = undefined

-- file: src/PrettyJSON.hs
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

-- file: src/PrettyJSON.hs
smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

    import Numeric (showHex)

-- file: src/PrettyJSON.hs
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n \`shiftR\` 10) .&. 0x3ff
          b = n .&. 0x3ff

-- file: src/PrettyJSON.hs
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

  -- file: src/PrettyJSON.hs
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item

-- file: src/Prettify.hs
fsep :: [Doc] -> Doc
fsep xs = undefined

-- file: src/Prettify.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds  

-- file: src/PrettyJSON.hs
renderJValue (JArray ary) = series '\[' '\]' renderJValue ary

-- file: src/PrettyJSON.hs
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val

-- file: src/PrettyJSON.hs
module PrettyJSON
    (
      renderJValue
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)

-- file: src/Prettify.hs
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)

-- file: src/Prettify.hs
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

-- file: src/Prettify.hs
line :: Doc
line = Line

-- file: src/Prettify.hs
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

-- file: src/Prettify.hs
concat :: [[a]] -> [a]
concat = foldr (++) []

-- file: src/Prettify.hs
hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- file: src/Prettify.hs
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

-- file: src/Prettify.hs
group :: Doc -> Doc
group x = flatten x \`Union\` x

-- file: src/Prettify.hs
flatten :: Doc -> Doc
flatten (x \`Concat\` y) = flatten x \`Concat\` flatten y
flatten Line           = Char ' '
flatten (x \`Union\` _)  = flatten x
flatten other          = other

-- file: src/Prettify.hs
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

-- file: src/Prettify.hs
pretty :: Int -> Doc -> String

-- file: src/Prettify.hs
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

-- file: src/Prettify.hs
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

-- file: app/Main.hs
module Main (main) where

import SimpleJSON
import Prettify
import PrettyJSON


value = renderJValue $ JObject [("f", JNumber 1), ("q", JBool True)]
main :: IO ()
main = do
    putStrLn (pretty 10 value)
    putStrLn (pretty 20 value)
    putStrLn (pretty 30 value)