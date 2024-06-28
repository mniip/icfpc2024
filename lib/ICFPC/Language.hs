module ICFPC.Language where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Primitive.PrimArray
import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Numeric.Natural


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
  | TString Text
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

parseInteger :: Attoparsec.Parser Natural
parseInteger = Attoparsec.takeWhile validChar
  <&> BS8.foldl' (\n c -> n * 94 + fromIntegral (charValue c)) 0

encoding :: PrimArray Char
encoding = primArrayFromList
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-\
  \./:;<=>?@[\\]^_`|~ \n"

parseString :: Attoparsec.Parser Text
parseString = Attoparsec.takeWhile validChar
  <&> BS8.map (indexPrimArray encoding . charValue)
  <&> T.decodeASCII

parseUnaryOp :: Attoparsec.Parser UnaryOp
parseUnaryOp = Attoparsec.anyChar >>= \case
  '-' -> pure Neg
  '!' -> pure Not
  '#' -> pure Int2Str
  '$' -> pure Str2Int
  c -> fail $ "parseUnaryOp " <> show c

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

decodeTokenStream :: ByteString -> [Token]
decodeTokenStream = either error id . Attoparsec.parseOnly do
  Attoparsec.sepBy parseToken (Attoparsec.char ' ') <* Attoparsec.endOfInput

data Expr
  = EFalse
  | ETrue
  | EInt Natural
  | EString Text
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

decodeExpr :: ByteString -> Expr
decodeExpr = either error id . Attoparsec.parseOnly do
  parseExpr <* Attoparsec.endOfInput
