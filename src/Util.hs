{-# LANGUAGE LambdaCase #-}
module Util (CompilationError, getStage,
             lookupElemIndex) where 

class Show e => CompilationError e where
  getStage :: e -> String 

-- | Looks up an item in an association list and 
-- | returns the corresponding index as well 
-- | lookupElemIndex "bar" [("foo", "free"), ("bar", "buzz")] => Just (1, "buzz")
lookupElemIndex :: (Eq a, Integral i) => a -> [(a, b)] -> Maybe (i, b)
lookupElemIndex = go 0 
  where go index item = \case [] -> Nothing 
                              (key, value):xs | item == key -> Just (index, value)
                                              | otherwise -> go (index + 1) item xs 