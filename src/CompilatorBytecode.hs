module CompilatorBytecode
  ( printByteCode,
    compileToByteCode,
    toByteString,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import Data.Int (Int64)
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64)
import Types

type ByteCode = ByteString

intToBytes :: Int64 -> [Word8]
intToBytes i = [fromIntegral (i `shiftR` (8 * k) .&. 0xff) | k <- [7, 6 .. 0]]

floatToBytes :: Float -> [Word8]
floatToBytes f =
  let word64 = castDoubleToWord64 $ realToFrac f
   in word64ToBytes word64

word64ToBytes :: Word64 -> [Word8]
word64ToBytes w = [fromIntegral (w `shiftR` (8 * i) .&. 0xff) | i <- [7, 6 .. 0]]

stringToBytes :: String -> [Word8]
stringToBytes str = map (fromIntegral . fromEnum) str ++ [0]

encodeInstruction :: Instruction -> ByteCode
encodeInstruction (IPushSym s) = pack (0x10 : stringToBytes s)
encodeInstruction (IPushInt i) = pack (0x11 : intToBytes (fromIntegral i))
encodeInstruction (IPushFloat f) = pack (0x12 : floatToBytes f)
encodeInstruction (IPushBool b) = pack (0x13 : [if b then 1 else 0])
encodeInstruction (IPushChar c) = pack (0x14 : [fromIntegral $ fromEnum c])
encodeInstruction (IPushString s) = pack (0x15 : stringToBytes s)
encodeInstruction IPushNull = pack [0x16]
encodeInstruction (IBuildList n) = pack (0x20 : intToBytes (fromIntegral n))
encodeInstruction IUnaryNot = pack [0x30]
encodeInstruction IUnaryMinus = pack [0x31]
encodeInstruction (IBinaryOp op) = pack (0x40 : [encodeBinaryOp op])
encodeInstruction (IJumpIfFalse n) = pack (0x50 : intToBytes (fromIntegral n))
encodeInstruction (IJump n) = pack (0x51 : intToBytes (fromIntegral n))
encodeInstruction (ICallFunc name n) = pack (0x60 : stringToBytes name ++ intToBytes (fromIntegral n))
encodeInstruction (IFuncDefStart name args) = pack (0x70 : stringToBytes name ++ concatMap stringToBytes args)
encodeInstruction (IFuncDefEnd name) = pack (0x71 : stringToBytes name)
encodeInstruction IStartLoop = pack [0x80]
encodeInstruction IEndLoop = pack [0x81]
encodeInstruction IReturn = pack [0x90]
encodeInstruction (IPrint) = pack [0xA0]

encodeBinaryOp :: BinaryOperator -> Word8
encodeBinaryOp PlusOp = 0x01
encodeBinaryOp MinusOp = 0x02
encodeBinaryOp Multiply = 0x03
encodeBinaryOp Divide = 0x04
encodeBinaryOp Modulo = 0x05
encodeBinaryOp And = 0x06
encodeBinaryOp Or = 0x07
encodeBinaryOp Equal = 0x08
encodeBinaryOp Equality = 0x09
encodeBinaryOp NotEqual = 0x0A
encodeBinaryOp LessThan = 0x0B
encodeBinaryOp LessThanOrEqual = 0x0C
encodeBinaryOp GreaterThan = 0x0D
encodeBinaryOp GreaterThanOrEqual = 0x0E

compileToByteCode :: [Instruction] -> ByteCode
compileToByteCode instrs = BS.concat $ map encodeInstruction instrs

toByteString :: ByteCode -> ByteString
toByteString = encode

printByteCode :: ByteString -> IO ()
printByteCode bytecode = putStrLn $ "Bytecode: " ++ show (encode bytecode)
