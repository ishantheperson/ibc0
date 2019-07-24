{-# LANGUAGE LambdaCase, Rank2Types #-}
module Util where 

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
-- Requires rank 2 types for Lens'  
updatePool :: Eq a => a -> Lens' b [a] -> (Int -> c) -> State b c 
updatePool elem lens f = do pool <- gets $ view lens 
                            case elemIndex elem pool of 
                              Just index -> return $ f index 
                              Nothing -> do modify (over lens (++[elem]))
                                            return . f $ length pool 

{-# INLINE lookupElemIndex #-}