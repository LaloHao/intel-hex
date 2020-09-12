{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
import Control.Monad (void)
import Control.Monad.State (State(..))
import qualified Control.Monad.State as State
import Data.Int (Int(..))
import Data.Map (Map(..))
import qualified Data.Map as Map
import GHC.TypeLits

emit _ = undefined
main = undefined

data Register
  = Indirect
  | TMR0
  | PCL
  | STATUS
  | FSR
  | PORTA
  | PORTB
  | PORTC
  | PORTD
  | PORTE
  | PCLATH
  | INTCON
  | PIR1
  | PIR2
  | TMR1L
  | TMR1H
  | T1CON
  | TMR2
  | T2CON
  | SSPBUF
  | SSPCON
  | CCPR1L
  | CCPR1H
  | CCP1CON
  | RCSTA
  | TXREG
  | RCREG
  | CCPR2L
  | CCPR2H
  | CCP2CON
  | ADRESH
  | ADCON0
  deriving (Read, Show, Eq, Ord, Enum)

type Value = Integer
data Address

data Instruction
  = CLRF Register
  | MOVF Register Register
  | MOVWF Register
  | GOTO Address

type StateRegister = Map Register Value
type CPUState = Map Register Value
-- type CPUState = State CPUState CPUState
-- type CPUUNit = State CPUState Unit
data CPU = State CPUState CPUState
  deriving (Eq, Show)

-- clrf :: State CPUState ()
-- clrf r = do
--   state <- State.get
--   return $ Map.insert r 0 state
-- set :: Register -> Value -> CPUState
-- set r v = do
--   state <- State.get
--   State.put $ Map.insert r v state
--   return v

-- eval :: Instruction -> State CPUState CPUState
-- eval (CLRF register) = set register 0

type family Reg a where
  Reg Register = Register
  Reg Value = Value
  Reg x = TypeError ('Text "Vot ze fuck man "
                      ':<>: 'ShowType x)

-- fun :: Reg "Nope"
-- fun = yes
