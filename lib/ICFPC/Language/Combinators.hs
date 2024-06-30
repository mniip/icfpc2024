module ICFPC.Language.Combinators where

import Data.List
import Data.Maybe
import Language.Haskell.TH.Syntax


baseDecoder :: String -> Q Exp
baseDecoder dictionary = [|\self n -> if n == 0
    then ""
    else
      let
        q = n `quot` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)
        r = n `rem` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)
      in take 1 (drop r $(pure $ LitE $ StringL dictionary)) <> self self q
  |]

baseDecoderDouble :: String -> Q Exp
baseDecoderDouble dictionary = [|\self n -> if n == 0
    then ""
    else
      let
        q = n `quot` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)
        r = (n `rem` $(pure $ LitE $ IntegerL $ toInteger $ length dictionary)) * 2
      in take 2 (drop r $(pure $ LitE $ StringL $ dictionary <* [(),()])) <> self self q
  |]

encodeBase :: String -> String -> Integer
encodeBase dictionary = go 0 1
  where
    go !acc !_ [] = acc
    go acc p (x:xs) = go
      (acc + p * toInteger (fromJust $ elemIndex x dictionary)) (p * b) xs
    !b = toInteger $ length dictionary

encodeBaseDouble :: String -> String -> Integer
encodeBaseDouble dictionary = go
  where
    go [] = 0
    go (x:x':xs)
      | x == x'
      = toInteger (fromJust $ elemIndex x dictionary)
        + toInteger (length dictionary) * go xs
    go xs = error $ take 2 xs
