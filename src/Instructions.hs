module Instructions where

-- Table 15-2: PIC16F883/884/886/887 INSTRUCTION SET
--           BYTE-ORIENTED FILE REGISTER OPERATIONS
-- Mnemonic        Description                  14-Bit Opcode    CLK
instructions = --                             MSb           LSb   |
 [(ADDWF  , "Add W and f"                  , "00 0111 dfff ffff", 1) -- (1, 2)
 ,(ANDWF  , "AND W with f"                 , "00 0101 dfff ffff", 1) -- (1, 2)
 ,(CLRF   , "Clear f"                      , "00 0001 1fff ffff", 1) -- (2)
 ,(CLRW   , "Clear W"                      , "00 0001 0xxx xxxx", 1)
 ,(COMF   , "Complement f"                 , "00 1001 dfff ffff", 1) -- (1, 2)
 ,(DECF   , "Decrement f"                  , "00 0011 dfff ffff", 1) -- (1, 2)
 ,(DECFSZ , "Decrement f, Skip if 0"       , "00 1011 dfff ffff", 2) -- (1, 2, 3)
 ,(INCF   , "Increment f"                  , "00 1010 dfff ffff", 1) -- (1, 2)
 ,(INCFSZ , "Increment f, Skip if 0"       , "00 1111 dfff ffff", 2) -- (1, 2, 3)
 ,(IORWF  , "Inclusive OR W with f"        , "00 0100 dfff ffff", 1) -- (1, 2)
 ,(MOVF   , "Move f"                       , "00 1000 dfff ffff", 1) -- (1, 2)
 ,(MOVWF  , "Move W to f"                  , "00 0000 1fff ffff", 1)
 ,(NOP    , "No Operation"                 , "00 0000 0xx0 0000", 1)
 ,(RLF    , "Rotate Left f through Carry"  , "00 1101 dfff ffff", 1) -- (1, 2)
 ,(RRF    , "Rotate Right f through Carry" , "00 1100 dfff ffff", 1) -- (1, 2)
 ,(SUBWF  , "Subtract W from f"            , "00 0010 dfff ffff", 1) -- (1, 2)
 ,(SWAPF  , "Swap nibbles in f"            , "00 1110 dfff ffff", 1) -- (1, 2)
 ,(XORWF  , "Exclusive OR W with f"        , "00 0110 dfff ffff", 1) -- (1, 2)
--         BIT-ORIENTED FILE REGISTER OPERATIONS
 ,(BCF    , "Bit Clear f"                  , "01 00bb bfff ffff", 1) -- (1, 2)
 ,(BSF    , "Bit Set f"                    , "01 01bb bfff ffff", 1) -- (1, 2)
 ,(BTFSC  , "Bit Test f, Skip if Clear"    , "01 10bb bfff ffff", 2) -- (3)
 ,(BTFSS  , "Bit Test f, Skip if Set"      , "01 11bb bfff ffff", 2) -- (3)
--            LITERAL AND CONTROL OPERATIONS
 ,(ADDLW  , "Add literal and W"            , "11 111x kkkk kkkk", 1)
 ,(ANDLW  , "AND literal and W"            , "00 0110 kkkk kkkk", 1)
 ,(CALL   , "Call Subroutine"              , "00 0110 kkkk kkkk", 2)
 ,(CLRWDT , "Clear Watchdog Timer"         , "00 0000 0110 0100", 1)
 ,(GOTO   , "Go to address"                , "10 1kkk kkkk kkkk", 2)
 ,(IORLW  , "Inclusive OR literal with W"  , "11 1000 kkkk kkkk", 1)
 ,(MOVLW  , "Move literal to W"            , "11 00xx kkkk kkkk", 1)
 ,(RETFIE , "Return from interrupt"        , "00 0000 0000 1001", 2)
 ,(RETLW  , "Return with literal in W"     , "11 01xx kkkk kkkk", 2)
 ,(RETURN , "Return from Subroutine"       , "00 0000 0000 1000", 2)
 ,(SLEEP  , "Go into Standby mode"         , "00 0000 0110 0011", 1)
 ,(SUBLW  , "Subtract W from literal"      , "11 111x kkkk kkkk", 1)
 ,(XORLW  , "Exclusive OR literal with W"  , "11 1010 kkkk kkkk", 1)]

-- 1. When an I/O register is modified as a function of itself (e.g. MOVF
-- GPIO1), the value used will be that value present on the pints themselves.
-- For example if the data latch is '1' for a pin configured as input and is
-- driven low by an external device, the data will be written back with a '0'

-- 2. When an instruction is executed on the TMR0 register (and wher
-- eapplicable, d=1), the prescaler will be cleared if asigned to the Timer0
-- module.

-- 3. If the Program Counter (PC) is modified, or a conditional test is true,
-- the instruction requires to cycles. The second cycle is executed as a NOP.

-- DS41291D-page 226                       (C) 2007 Microchip Technology Inc.
