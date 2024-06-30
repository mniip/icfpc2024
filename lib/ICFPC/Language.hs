module ICFPC.Language where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Primitive.PrimArray
import Data.Functor
import Data.List
import Data.String
import Data.Word
import Numeric.Natural


newtype ICFPText = ICFPText ByteString
  deriving stock (Eq, Ord)

icfpToByteString :: ICFPText -> ByteString
icfpToByteString (ICFPText bs) = BS.map (indexPrimArray encoding . fromEnum) bs

encoding :: PrimArray Word8
encoding = primArrayFromList $ toEnum . fromEnum <$>
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-\
  \./:;<=>?@[\\]^_`|~ \n"

decoding :: PrimArray Word8
decoding = primArrayFromList
  [ case findIndex (== c) $ primArrayToList encoding of
    Just i -> toEnum i
    Nothing -> 0xFF
  | c <- [0..127] ]

icfpFromByteString :: ByteString -> ICFPText
icfpFromByteString = ICFPText . BS.map (indexPrimArray decoding . fromEnum)

instance Show ICFPText where
  showsPrec d str
    | '\n' `elem` decoded = showString "\"\\\n\\"
      . showString formatted
      . showString "\\\n\\\""
    | otherwise = showsPrec d  decoded
    where
      decoded = BS8.unpack $ icfpToByteString str
      formatted = decoded >>= \case
        '\n' -> "\\n\\\n\\"
        '"' -> "\""
        '\'' -> "'"
        c -> let w = show c in drop 1 $ zipWith const w $ drop 1 w

instance IsString ICFPText where
  fromString = icfpFromByteString . BS8.pack

data UnaryOp
  = Neg
  | Not
  | Int2Str
  | Str2Int
  deriving stock (Eq, Ord, Show)

data BinaryOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Less
  | Greater
  | Equal
  | Or
  | And
  | Concat
  | Take
  | Drop
  | Apply
  deriving stock (Eq, Ord, Show)

data Token
  = TBool Bool
  | TInt Natural
  | TString ICFPText
  | TUnary UnaryOp
  | TBinary BinaryOp
  | TIf
  | TLambda Natural
  | TVar Natural
  deriving stock (Eq, Ord, Show)

validChar :: Char -> Bool
validChar c = c >= '!' && c <= '~'

charValue :: Char -> Int
charValue c = fromEnum c - fromEnum '!'

unCharValue :: Int -> Char
unCharValue i = toEnum (i + fromEnum '!')

icfpToInt :: ByteString -> Natural
icfpToInt = BS.foldl' (\n c -> n * 94 + fromIntegral c) 0

icfpFromInt :: Natural -> ByteString
icfpFromInt 0 = BS.pack [0]
icfpFromInt k = BS.pack $ go k []
  where
    go 0 xs = xs
    go n xs = case divMod n 94 of
      (d, m) -> go d (fromIntegral m : xs)

parseInteger :: Attoparsec.Parser Natural
parseInteger = Attoparsec.takeWhile validChar
  <&> BS.map (subtract $ fromIntegral $ fromEnum '!')
  <&> icfpToInt

formatInteger :: Natural -> Builder
formatInteger = B.byteString
  . BS.map (+ fromIntegral (fromEnum '!')) . icfpFromInt

parseString :: Attoparsec.Parser ICFPText
parseString = Attoparsec.takeWhile validChar
  <&> BS.map (subtract $ fromIntegral $ fromEnum '!')
  <&> ICFPText


formatString :: ICFPText -> Builder
formatString (ICFPText bs) = B.byteString
  $ BS.map (+ fromIntegral (fromEnum '!')) bs

parseUnaryOp :: Attoparsec.Parser UnaryOp
parseUnaryOp = Attoparsec.anyChar >>= \case
  '-' -> pure Neg
  '!' -> pure Not
  '#' -> pure Str2Int
  '$' -> pure Int2Str
  c -> fail $ "parseUnaryOp " <> show c

formatUnaryOp :: UnaryOp -> Builder
formatUnaryOp = \case
  Neg -> B.char8 '-'
  Not -> B.char8 '!'
  Int2Str -> B.char8 '#'
  Str2Int -> B.char8 '$'

parseBinaryOp :: Attoparsec.Parser BinaryOp
parseBinaryOp = Attoparsec.anyChar >>= \case
  '+' -> pure Add
  '-' -> pure Subtract
  '*' -> pure Multiply
  '/' -> pure Divide
  '%' -> pure Modulo
  '<' -> pure Less
  '>' -> pure Greater
  '=' -> pure Equal
  '|' -> pure Or
  '&' -> pure And
  '.' -> pure Concat
  'T' -> pure Take
  'D' -> pure Drop
  '$' -> pure Apply
  c -> fail $ "parseBinaryOp" <> show c

formatBinaryOp :: BinaryOp -> Builder
formatBinaryOp = \case
  Add -> B.char8 '+'
  Subtract -> B.char8 '-'
  Multiply -> B.char8 '*'
  Divide -> B.char8 '/'
  Modulo -> B.char8 '%'
  Less -> B.char8 '<'
  Greater -> B.char8 '>'
  Equal -> B.char8 '='
  Or -> B.char8 '|'
  And -> B.char8 '&'
  Concat -> B.char8 '.'
  Take -> B.char8 'T'
  Drop -> B.char8 'D'
  Apply -> B.char8 '$'

parseToken :: Attoparsec.Parser Token
parseToken = Attoparsec.anyChar >>= \case
  'F' -> pure $ TBool False
  'T' -> pure $ TBool True
  'I' -> TInt <$> parseInteger
  'S' -> TString <$> parseString
  'U' -> TUnary <$> parseUnaryOp
  'B' -> TBinary <$> parseBinaryOp
  '?' -> pure TIf
  'L' -> TLambda <$> parseInteger
  'v' -> TVar <$> parseInteger
  c -> fail $ "parseToken " <> show c

formatToken :: Token -> Builder
formatToken = \case
  TBool False -> B.char8 'F'
  TBool True -> B.char8 'T'
  TInt x -> B.char8 'I' <> formatInteger x
  TString x -> B.char8 'S' <> formatString x
  TUnary x -> B.char8 'U' <> formatUnaryOp x
  TBinary x -> B.char8 'B' <> formatBinaryOp x
  TIf -> B.char8 '?'
  TLambda i -> B.char8 'L' <> formatInteger i
  TVar i -> B.char8 'v' <> formatInteger i

decodeTokenStream :: ByteString -> [Token]
decodeTokenStream = either error id . Attoparsec.parseOnly do
  Attoparsec.sepBy parseToken (Attoparsec.char ' ') <* Attoparsec.endOfInput

encodeTokenStream :: [Token] -> ByteString
encodeTokenStream [] = mempty
encodeTokenStream (x0:xs0) = BSL.toStrict $ B.toLazyByteString
  $ formatToken x0 <> go xs0
  where
    go [] = mempty
    go (x:xs) = B.char8 ' ' <> formatToken x <> go xs

data Expr
  = EBool Bool
  | EInt Integer -- never negative in syntax, sometimes in intermediate values
  | EString ICFPText
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | If Expr Expr Expr
  | Lambda Natural Expr
  | Var Natural
  deriving stock (Eq, Ord, Show)

parseExpr :: Attoparsec.Parser Expr
parseExpr = do
  void $ optional $ Attoparsec.char ' '
  parseToken >>= \case
    TBool b -> pure $ EBool b
    TInt x -> pure $ EInt $ toInteger x
    TString x -> pure $ EString x
    TUnary op -> Unary op <$> parseExpr
    TBinary op -> Binary op <$> parseExpr <*> parseExpr
    TIf -> If <$> parseExpr <*> parseExpr <*> parseExpr
    TLambda i -> Lambda i <$> parseExpr
    TVar i -> pure $ Var i

formatExpr :: Expr -> Builder
formatExpr = \case
  EBool x -> formatToken $ TBool x
  EInt x -> formatToken $ TInt $ fromInteger x
  EString x -> formatToken $ TString x
  Unary op x -> formatToken (TUnary op) <> B.char8 ' ' <> formatExpr x
  Binary op x y -> formatToken (TBinary op) <> B.char8 ' ' <> formatExpr x
    <> B.char8 ' ' <> formatExpr y
  If x y z -> B.char8 '?' <> B.char8 ' ' <> formatExpr x
    <> B.char8 ' ' <> formatExpr y
    <> B.char8 ' ' <> formatExpr z
  Lambda i x -> formatToken (TLambda i) <> B.char8 ' ' <> formatExpr x
  Var i -> formatToken (TVar i)

decodeExpr :: ByteString -> Expr
decodeExpr = either error id . Attoparsec.parseOnly do
  parseExpr <* Attoparsec.endOfInput

encodeExpr :: Expr -> ByteString
encodeExpr = BSL.toStrict . B.toLazyByteString . formatExpr

codeSize :: Expr -> Int
codeSize = BS.length . encodeExpr
