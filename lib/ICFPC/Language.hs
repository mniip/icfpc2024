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

encoding :: PrimArray Char
encoding = primArrayFromList
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-\
  \./:;<=>?@[\\]^_`|~ \n"

instance Show ICFPText where
  showsPrec d (ICFPText pa)
    | '\n' `elem` decoded = showString "\"\\\n\\"
      . showString formatted
      . showString "\\\n\\\""
    | otherwise = showsPrec d  decoded
    where
      decoded = map (indexPrimArray encoding . fromEnum) $ BS.unpack pa
      formatted = decoded >>= \case
        '\n' -> "\\n\\\n\\"
        '"' -> "\""
        '\'' -> "'"
        c -> let w = show c in drop 1 $ zipWith const w $ drop 1 w

decoding :: PrimArray Word8
decoding = primArrayFromList
  [ case findIndex (== c) $ primArrayToList encoding of
    Just i -> toEnum i
    Nothing -> 0xFF
  | c <- take 128 [minBound..] ]

instance IsString ICFPText where
  fromString = ICFPText . BS.map (indexPrimArray decoding . fromEnum) . BS8.pack

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
  = TFalse
  | TTrue
  | TInt Natural
  | TString ICFPText
  | TUnary UnaryOp
  | TBinary BinaryOp
  | TTernary
  | TLambda Natural
  | TVar Natural
  deriving stock (Eq, Ord, Show)

validChar :: Char -> Bool
validChar c = c >= '!' && c <= '~'

charValue :: Char -> Int
charValue c = fromEnum c - fromEnum '!'

unCharValue :: Int -> Char
unCharValue i = toEnum (i + fromEnum '!')

parseInteger :: Attoparsec.Parser Natural
parseInteger = Attoparsec.takeWhile validChar
  <&> BS8.foldl' (\n c -> n * 94 + fromIntegral (charValue c)) 0

formatInteger :: Natural -> Builder
formatInteger = \case
  0 -> B.char8 (unCharValue 0)
  m -> go m mempty
  where
    go 0 b = b
    go n b = case divMod n 94 of
      (d, m) -> go d (B.char8 (unCharValue $ fromIntegral m) <> b)

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
  '#' -> pure Int2Str
  '$' -> pure Str2Int
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
  'F' -> pure TFalse
  'T' -> pure TTrue
  'I' -> TInt <$> parseInteger
  'S' -> TString <$> parseString
  'U' -> TUnary <$> parseUnaryOp
  'B' -> TBinary <$> parseBinaryOp
  '?' -> pure TTernary
  'L' -> TLambda <$> parseInteger
  'v' -> TVar <$> parseInteger
  c -> fail $ "parseToken " <> show c

formatToken :: Token -> Builder
formatToken = \case
  TFalse -> B.char8 'F'
  TTrue -> B.char8 'T'
  TInt x -> B.char8 'I' <> formatInteger x
  TString x -> B.char8 'S' <> formatString x
  TUnary x -> B.char8 'U' <> formatUnaryOp x
  TBinary x -> B.char8 'B' <> formatBinaryOp x
  TTernary -> B.char8 '?'
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
  = EFalse
  | ETrue
  | EInt Natural
  | EString ICFPText
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Ternary Expr Expr Expr
  | Lambda Natural Expr
  | Var Natural
  deriving stock (Eq, Ord, Show)

parseExpr :: Attoparsec.Parser Expr
parseExpr = do
  void $ optional $ Attoparsec.char ' '
  parseToken >>= \case
    TFalse -> pure EFalse
    TTrue -> pure ETrue
    TInt x -> pure $ EInt x
    TString x -> pure $ EString x
    TUnary op -> Unary op <$> parseExpr
    TBinary op -> Binary op <$> parseExpr <*> parseExpr
    TTernary -> Ternary <$> parseExpr <*> parseExpr <*> parseExpr
    TLambda i -> Lambda i <$> parseExpr
    TVar i -> pure $ Var i

formatExpr :: Expr -> Builder
formatExpr = \case
  EFalse -> formatToken TFalse
  ETrue -> formatToken TTrue
  EInt x -> formatToken $ TInt x
  EString x -> formatToken $ TString x
  Unary op x -> formatToken (TUnary op) <> B.char8 ' ' <> formatExpr x
  Binary op x y -> formatToken (TBinary op) <> B.char8 ' ' <> formatExpr x
    <> B.char8 ' ' <> formatExpr y
  Ternary x y z -> B.char8 '?' <> B.char8 ' ' <> formatExpr x
    <> B.char8 ' ' <> formatExpr y
    <> B.char8 ' ' <> formatExpr z
  Lambda i x -> formatToken (TLambda i) <> B.char8 ' ' <> formatExpr x
  Var i -> formatToken (TVar i)

decodeExpr :: ByteString -> Expr
decodeExpr = either error id . Attoparsec.parseOnly do
  parseExpr <* Attoparsec.endOfInput

encodeExpr :: Expr -> ByteString
encodeExpr = BSL.toStrict . B.toLazyByteString . formatExpr
