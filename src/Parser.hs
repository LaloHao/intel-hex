{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser (operand, instruction, record, parse, test,) where
import Number (fromHex, binaryDigits, hexDigits, toHex)
import Util (checksum)
import Types (enumT, Record, toRecord, Operand(..))
import Control.Arrow ((***))
import Control.Monad (void)
import Text.Parsec (char, many1, oneOf, string, try, parserFail, newline, ParsecT)
import Text.Parsec.String (Parser, parseFromFile)
-- import Text.Parsec.Combinator (fail)
import qualified Text.Parsec as P
import Prelude hiding (sum)

hex :: Int -> Parser Int
hex n = do
  value <- P.count n $ oneOf hexDigits
  return (fromHex value)

type Byte = Int
byte :: Parser Byte
byte = hex 2
bytes :: Int -> Parser [Byte]
bytes = flip P.count byte

record :: Parser Record
record = do
  void $ char ':'
  n <- count
  a <- address
  tc <- recordType
  d <- info n
  c <- chksum [n, a, [fromEnum tc], d]
  return $ toRecord n a tc d c
  where
    count = bytes 1
    address = bytes 2
    info = bytes . head
    -- recordType = (<|>) $ (\(a, b) -> b <$ try a) <$> ((string . toHex) *** id) <$> enumT
    recordType = (<|>) $ ((string . toHex) *** id) <$> enumT
    chksum a = do
      chk <- string . toHex $ checksum a
      return [fromHex chk]

(<|>) :: (Foldable t, Functor t) => t (ParsecT s u m a, b) -> ParsecT s u m b
(<|>) p = foldl (P.<|>) (parserFail "") parsers
  where parsers = toParser <$> p
        toParser (a, b) = b <$ try a

line :: Parser a -> Parser a
line p = do { c <- p; void $ newline; return c }

parseFile :: String -> IO [Record]
parseFile file = parseFromFile (many1 $ line record) file >>= either none some
  where
    none err = do
      putStrLn $ "Error: " ++ show err
      return []
    some m = do
      return m

parse :: Parser a -> String -> Either P.ParseError a
parse p s = P.runP p () "" s

test :: Show a => Parser a -> String -> IO ()
test = P.parseTest

-- instruction :: Parser (String, String)
-- instruction = do
--   op <- opcode
--   args <- operands
--   return (padR 14 '0' op, args)
--   -- return op
--   where
--     opcode :: Parser String
--     opcode = many1 $ oneOf binaryDigits
--         -- operands :: Parser String
instruction = do
  instr <- P.count 14 $ get1
  return instr
  where
    get1 = foldl (P.<|>) (parserFail "") [operand]

operand :: Parser Operand
operand
  = (<|>) [(f,F),(d,D),(b,B),(x,X),(k,K),(i,I),(o,I)]
  where f = char 'f'
        d = char 'd'
        b = char 'b'
        x = char 'x'
        k = char 'k'
        i = char '1'
        o = char '0'

-- operands :: Parser [Operand]
-- operands
--   = (<|>) $ [(fd, [D,F]),(f, [F]),(fb, [F, B]),(xk, [X,K]),(k, [K])]
--   where f = P.count 7 $ char 'f'
--         d = P.count 1 $ char 'd'
--         b = P.count 3 $ char 'b'
--         x = P.many1   $ char 'x'
--         k = P.count 8 $ char 'k'
--         fd = do
--           ds <- d
--           fs <- f
--           return $ ds<>fs
--         fb = do
--           bs <- b
--           fs <- f
--           return $ bs<>fs
--         xk = do
--           xs <- x
--           ks <- k
--           return $ xs<>ks
