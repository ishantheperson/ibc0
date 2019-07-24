{-# LANGUAGE LambdaCase, Rank2Types #-}
module Util where 

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Lens 
import Control.Monad.State 


-- | Represents a description of something that went wrong
class Show e => CompilationError e where
  getStage :: e -> String 

-- | Looks up an item in an association list and 
--   returns the corresponding index as well 
--  
-- >>> lookupElemIndex "bar" [("foo", "free"), ("bar", "buzz")] =
-- Just (1, "buzz")
lookupElemIndex :: (Eq a, Integral i) => a -> [(a, b)] -> Maybe (i, b)
lookupElemIndex = go 0 
  where go index item = \case [] -> Nothing 
                              (key, value):xs | item == key -> Just (index, value)
                                              | otherwise -> go (index + 1) item xs 

{-# INLINE lookupElemIndex #-}

-- Requires rank 2 types for Lens'  
updatePool :: Eq a => a -> Lens' b [a] -> (Int -> c) -> State b c 
updatePool elem lens f = do pool <- gets $ view lens 
                            case elemIndex elem pool of 
                              Just index -> return $ f index 
                              Nothing -> do modify (over lens (++[elem]))
                                            return . f $ length pool 

same :: Eq a => [a] -> Bool 
same = and . (zipWith (==) <*> tail)

-- {-# INLINE same #-}

sbyteToHex, ubyteToHex, ushortToHex, sshortToHex, intToHex :: Int -> String 
sbyteToHex i = printf "%02X" (if i < 0 then i + (2^8) else i)
ubyteToHex i = printf "%02X" i 

ushortToHex i = addSpaces (printf "%04X" i) 
sshortToHex i = addSpaces (printf "%04X" (if i < 0 then i + (2^16) else i)) 

-- signed 32-bit int to hex
intToHex i = addSpaces (printf "%08X" (if i < 0 then i + (2^32) else i))

-- {-# INLINE sbyteToHex, ubyteToHex, ushortToHex, sshortToHex, intToHex #-}

-- adds spaces every 2 characters 
-- e.g. 1234 -> 12 34 
addSpaces :: String -> String 
addSpaces = \case 
  [] -> []
  a:[] -> a:[]
  a:b:[] -> a:b:[]
  a:b:xs -> a:b:' ':(addSpaces xs)

