module ICFPC.Language.Combinators where

import ICFPC.Language
import ICFPC.Language.TH
import Language.Haskell.TH.Syntax


lambdaman1 :: Expr
lambdaman1 = $(lift . fromHaskell =<< [|
    "solve lambdaman1 RRURRLLDLLLLLDURRRU"
  |])

lambdaman4 :: Expr
lambdaman4 = $(lift . fromHaskell =<< [|
    "solve lambdaman4 " <>
    let go = $(baseDecoder "UDLR")
    in go go $(lift $ encodeBase "UDLR" "RRLLLLLLUULLUURRLLDDRRDDRRUUUUDDDDRRDDLLRRUUUURRRRDDDDRRRRLLUURRRRRRLLDDRRDDLLLLRRDDLLLLLLRRDDRRDDLLLLRRRRDDDDRRRRLLUURRLLDDLLUUUUUULLUUUUDDRRRRUURRDDDDLLRRDDLLRRUUUUUUUULLUULLLLDDLLLLRRDDLLDDLLUUDDRRDDRRLLDDLLDDDDUUUUUULLDDDDLLLLDDUUUURRUUDDLLDDRRRRDDLLRRUUUUUUUULLLLDDUUUUUUUUDDDDDDRRUURRLLUUDDDDRRDDRRDDRRDDRRRRDDUULLDDUULLDDUUUUUUUUUURRUUUUUULLLLUURRRRRRRRLLDDRRRRRRLLUURR")
  |])

lambdaman5 :: Expr -- not optimal
lambdaman5 = $(lift . fromHaskell =<< [|
    "solve lambdaman5 " <> let
      thrice f x = f (f (f x))
      times27 x = thrice thrice (\y -> y <> x) ""
      p = times27 "L" <> times27 "R"
      q = times27 "R" <> times27 "L"
    in
    times27 $
      times27 (p <> "D" <> q <> "D") <> "U"
      <> times27 (q <> "U" <> p <> "U") <> "D"
  |])

lambdaman6 :: Expr
lambdaman6 = $(lift . fromHaskell =<< [|
    "solve lambdaman6 " <> let
      triple x = x <> x <> x
    in triple $ triple $ triple "RRRRRRRR"
  |])
