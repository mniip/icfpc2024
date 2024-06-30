module ICFPC.Language.Combinators where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Language.Haskell.TH.Syntax
import ICFPC.Language

import Debug.Trace


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

factorOut :: [ByteString] -> ByteString -> Expr
factorOut pats bs0 = case foldl' go (0, id, [Right bs0]) pats of
  (_, wrap, chunks) -> wrap $ foldr1 (Binary Concat)
    $ either Var (EString . icfpFromByteString) <$> chunks
  where
    go (fresh, wrap, chunks) pat =
      ( fresh + 1
      , \t -> Binary Apply
        (Lambda fresh $ wrap t)
        (EString $ icfpFromByteString pat)
      , chunks >>= \case
        Left i -> pure $ Left i
        Right bs -> separateByteString fresh pat bs
      )

separateByteString :: a -> ByteString -> ByteString -> [Either a ByteString]
separateByteString x !pat !bs
  | BS.null bs = []
  | (h, t) <- BS.breakSubstring pat bs
  = if
    | BS.null t -> [Right h]
    | BS.null h -> Left x : separateByteString x pat (BS.drop (BS.length pat) t)
    | otherwise
      -> Right h : Left x : separateByteString x pat (BS.drop (BS.length pat) t)

data Ptr = Ptr !Int !Int {-# UNPACK #-} !ByteString

allInfixes :: [ByteString] -> [(ByteString, Int, Int)]
allInfixes bss0 = foldMap (go 1) (NE.nonEmpty $ part 0 $ mkPtrs bss0)
  where
    go !i groups = (toList groups <&> \g ->
        ( takePtr i $ NE.head g
        , length g
        , consecutivePtrs i g
        ))
      ++ foldMap (go (i + 1)) (NE.nonEmpty $ toList groups >>= part i)

    part i grp = mapMaybe (large . removeOverlaps . reverse . toList) $ M.elems
      $ M.fromListWith (<>)
        [ (c, NE.singleton p)
        | p <- toList grp
        , c <- maybeToList $ indexPtr i p
        ]
      where
        removeOverlaps [] = []
        removeOverlaps (x:xs) = x : walk x xs
          where
            walk p@(Ptr k j _) (q@(Ptr k' j' _) : ys)
              | k /= k' = q : walk q ys
              | j' < j + i = walk p ys
              | otherwise = q : walk q ys
            walk _ [] = []

    large (x:xs@(_:_)) = Just $ x:|xs
    large _ = Nothing

    mkPtrs bss =
      [ Ptr i j bs
      | (i, bs) <- zip [0..] bss
      , j <- [0 .. BS.length bs - 1]
      ]

    indexPtr i (Ptr _ off bs)
      | BS.length bs > off + i = Just $ BS.head $ BS.drop (off + i) bs
      | otherwise = Nothing
    takePtr i (Ptr _ off bs) = BS.take i $ BS.drop off bs

    consecutivePtrs i (x0 :| xs0)
      = walk (endCheck x0) x0 xs0
      where
        startCheck (Ptr _ off _) = if off == 0 then 1 else 0
        endCheck (Ptr _ off bs) = if off + i == BS.length bs then 1 else 0
        walk !acc p [] = acc + endCheck p
        walk !acc p@(Ptr k off _) (p'@(Ptr k' off' _) : xs)
          | k /= k' = walk (acc + endCheck p + startCheck p') p' xs
          | off' == off + i = walk (acc + 1) p' xs
          | otherwise = walk acc p' xs

compress :: ByteString -> [ByteString]
compress bs0 = go 0 [] [bs0]
  where
    wins binds len occ edgeOcc
      | binds < (94 :: Int)
      = occ * (len - 11) + edgeOcc * 5 - (len + 8)
      | otherwise
      = occ * (len - 12) + edgeOcc * 5 - (len + 9)

    go !binds revMap terms = case filter ((> 0) . snd)
        $ allInfixes terms <&> \(bs, occ, edgeOcc)
        -> (bs, wins binds (BS.length bs) occ edgeOcc)
      of
        [] -> reverse revMap
        r -> let
            (pat, w) = maximumBy (comparing snd) r
          in trace ("Won " <> show w <> " with " <> show pat)
            $ go (binds + 1) (pat:revMap) $ terms >>=
              mapMaybe either2Maybe . separateByteString () pat

    either2Maybe = either (const Nothing) Just

numOccurs :: ByteString -> ByteString -> Int
numOccurs = go 0
  where
    go !acc !pat !bs
      | BS.null bs = acc
      | (_, t) <- BS.breakSubstring pat bs
      , not $ BS.null t
      = go (acc + 1) pat (BS.drop (BS.length pat) t)
      | otherwise = acc
