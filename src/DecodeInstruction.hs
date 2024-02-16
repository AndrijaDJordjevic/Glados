-- EPITECH PROJECT, 2024
-- Glados [WSL: Ubuntu]
-- File description:
-- DecodeInstruction
--

module DecodeInstruction
(
    decodeInstruction,
    getString,
    word8ToBinaryOp
)where

import Data.Word (Word8)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Get (Get, getWord8, getInt64le, getFloatle, getByteString, runGetOrFail)
import Data.Int ()
import Types
import Control.Monad (replicateM)

decodeInstruction :: ByteString -> Maybe Instruction
decodeInstruction bs = case runGetOrFail getInstruction (LBS.fromStrict bs) of
    Left _ -> Nothing
    Right (_, _, instruction) -> Just instruction

getInstruction :: Get Instruction
getInstruction = do
    opcode <- getWord8
    case opcode of
        0x10 -> do
            s <- getString
            return $ IPushSym s
        0x11 -> do
            i <- getInt64le
            return $ IPushInt (fromIntegral i)
        0x12 -> do
            f <- getFloatle
            return $ IPushFloat f
        0x13 -> do
            boolVal <- getWord8
            return $ IPushBool (boolVal /= 0)
        0x14 -> do
            charVal <- getWord8
            return $ IPushChar (toEnum . fromIntegral $ charVal)
        0x15 -> do
            s <- getString
            return $ IPushString s
        0x16 -> return IPushNull
        0x20 -> do
            n <- getInt64le
            return $ IBuildList (fromIntegral n)
        0x30 -> return IUnaryNot
        0x31 -> return IUnaryMinus
        0x40 -> do
            opCode <- getWord8
            return $ IBinaryOp (word8ToBinaryOp opCode)
        0x50 -> do
            n <- getInt64le
            return $ IJumpIfFalse (fromIntegral n)
        0x51 -> do
            n <- getInt64le
            return $ IJump (fromIntegral n)
        0x60 -> do
            name <- getString
            argCount <- getWord8
            return $ ICallFunc name (fromIntegral argCount)
        0x70 -> do
            name <- getString
            argsCount <- getWord8
            args <- replicateM (fromIntegral argsCount) getString
            return $ IFuncDefStart name args
        0x71 -> do
            name <- getString
            return $ IFuncDefEnd name
        0x80 -> return IStartLoop
        0x81 -> return IEndLoop
        0x90 -> return IReturn
        _    -> fail "Unknown instruction"

getString :: Get String
getString = do
    len <- getWord8
    bs <- getByteString (fromIntegral len)
    return $ map (toEnum . fromIntegral) (BS.unpack bs)

word8ToBinaryOp :: Word8 -> BinaryOperator
word8ToBinaryOp 0x01 = PlusOp
word8ToBinaryOp 0x02 = MinusOp
word8ToBinaryOp 0x03 = Multiply
word8ToBinaryOp 0x04 = Divide
word8ToBinaryOp 0x05 = Modulo
word8ToBinaryOp 0x06 = And
word8ToBinaryOp 0x07 = Or
word8ToBinaryOp 0x08 = Equal
word8ToBinaryOp 0x09 = Equality
word8ToBinaryOp 0x0A = NotEqual
word8ToBinaryOp 0x0B = LessThan
word8ToBinaryOp 0x0C = LessThanOrEqual
word8ToBinaryOp 0x0D = GreaterThan
word8ToBinaryOp 0x0E = GreaterThanOrEqual
word8ToBinaryOp _    = error "Unknown binary operator"
