
module Hash
  ( Hash (Hash)
  , showAsHex
  , fastToHexEnum
  )
where

import Data.Bits

-- ---------------------------------------------------------------------------------------------------------------------

newtype Hash = Hash Int

instance Show Hash where
  show (Hash fp) = "0x" ++ showAsHex fp
  {-# INLINE show #-}

showAsHex :: Int -> String
showAsHex = go 8
  where
    go :: Int -> Int -> String
    go i n
      | i <= 0 = [fastToHexEnum (n .&. 0xF)]
      | otherwise = fastToHexEnum (rotateR n (i * 4) .&. 0xF) : go (i - 1) n
{-# INLINE showAsHex #-}

fastToHexEnum :: Int -> Char
fastToHexEnum n
  | n <= 9 = toEnum (n + 48)
  | otherwise = toEnum (n + 87)
{-# INLINE CONLIKE fastToHexEnum #-}
