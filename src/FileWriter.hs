{-
-- EPITECH PROJECT, 2023
-- Glados
-- File description:
-- FileWriter
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module FileWriter (saveToFile) where

import qualified Data.ByteString.Char8 as BS
import Types (Instruction)

-- import System.IO (appendFile)
-- import System.IO (writeFile)

class SaveToFile a where
  saveToFile :: FilePath -> a -> IO ()

instance SaveToFile BS.ByteString where
  saveToFile :: FilePath -> BS.ByteString -> IO ()
  saveToFile filePath content = writeFile filePath (BS.unpack content)

instance SaveToFile [Instruction] where
  saveToFile :: FilePath -> [Instruction] -> IO ()
  saveToFile filePath instructions = writeFile filePath (unlines $ map show instructions)

-- Instanciation de la classe de type pour les types qui sont instances de Show
-- instance (Show a) => SaveToFile a where
--   saveToFile filePath content = writeFile filePath (show content)
