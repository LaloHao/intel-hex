module Types
  (toRecord
  , Record
  , Address
  , Data
  , Type
  , Addressing
  , enumT
  , Register
  , Operand(..)
  , OPCode(..)
  , Instruction(..)
  , Mnemonic(..)) where
import Prelude hiding (sum)
import Util (checksum, sum, swap)
import Data.Maybe (fromJust)
import Number (toHex)
import Data.List (intersperse)

typeT :: [Type]
typeT = [Data, EOF, Extended Segment, Start Segment, Extended Linear, Start Linear]
enumT :: [(Int, Type)]
enumT = zip [0..] typeT

data Record
  = Record {
    _addr :: Address
  , _type :: Type
  , _data :: Data }
  deriving (Eq)

instance Show Record where
  show (Record a t d) = unwords $ [show a, show t, concat $ toHex <$> d, "\n"]

instance Show Address where
  show (Address a) =  concat $ toHex <$> a

-- instance Show Data where
--   show (Data d) = concat $ toHex <$> d

data Address = Address [Int]
  deriving (Eq)

type Data = [Int]

data Type
  = Data
  | EOF
  | Extended Addressing
  | Start Addressing
  deriving (Eq, Show, Ord)

data Addressing
  = Segment
  | Linear
  deriving (Eq, Show, Ord, Enum)

instance (Enum Type) where
  fromEnum = fromJust . flip lookup (swap <$> enumT)
  toEnum n
    | 0 <= n && n <= 5 = fromJust $ lookup n enumT
    | otherwise = error ("Could not match record type " <> toHex n)

toRecord :: [Int] -> [Int] -> Type -> Data -> [Int] -> Record
toRecord n a t d c = new
  where
    new = case checksum [n, a, [fromEnum t], d] == sum [c] of
      True -> Record (Address a) t d
      False -> error ":("

-- data family Operand a :: Char -> *

data Register
data OPCode = Int
data Operand = F | D | X | K | B | I | O
  deriving (Eq, Show, Ord)
newtype Mnemonic = Mnemonic String
data Instruction
  = Instruction
  { _opcode :: OPCode
  , _operands :: [Operand]
  , _mnemonic :: Mnemonic
  , _description :: String
  }
